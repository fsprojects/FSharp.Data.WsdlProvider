module FSharp.Data.WsdlProviderImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open MyNamespace
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open System.ServiceModel
open FSharp.Data.Wsdl
open System.Threading.Tasks

module String =
    let camlCase (s: string) =
        if s.Length >= 1 then
            string (Char.ToLowerInvariant(s.[0])) + s.Substring(1);
        else
            s

module Provided = 
    open System.Xml.Serialization
    type private ProvidedAttribute<'t>(args : (Type * obj) list, namedArgs: (string * obj) list)  = 
        inherit CustomAttributeData()
            override _.Constructor = typeof<'t>.GetConstructor(args |> Seq.map fst |> Seq.toArray)
            override _.ConstructorArguments = 
                upcast [| for t,v in args -> CustomAttributeTypedArgument(t,v) |]
            override _.NamedArguments =  
                upcast [| for p,v in namedArgs -> CustomAttributeNamedArgument(typeof<'t>.GetProperty(p), v ) |]
        
    let mkProvidedAttribute<'t> args namedArgs =
        ProvidedAttribute<'t>(args, namedArgs) :> CustomAttributeData

    let mkXmlElementAttribute (order: int) =  
        mkProvidedAttribute<XmlElementAttribute> [] [ "Order", box order ]


    let mkXmlTypeAttribute (ns: string) =
        mkProvidedAttribute<XmlTypeAttribute> [] ["Namespace", box ns]
  
    let mkServiceContractAttribute (ns: string) (configName: string) =
        mkProvidedAttribute<ServiceContractAttribute> [] ["Namespace", box ns; "ConfigurationName", box configName ] 

    let mkXmlSerializerFormatAttribute() =
        mkProvidedAttribute<XmlSerializerFormatAttribute> [] [ "SupportFaults", box true]
    
    let mkOperationContractAttribute (action: string) (replyAction: string) =
        mkProvidedAttribute<OperationContractAttribute> [] [ "Action", box action; "ReplyAction", box replyAction]
    
    let defineCtors (client: ProvidedTypeDefinition) location =
            let parentCtor =
                client.BaseType.GetConstructor(
                    BindingFlags.NonPublic ||| BindingFlags.Instance, 
                    null,
                    [| typeof<System.ServiceModel.Channels.Binding>
                       typeof<System.ServiceModel.EndpointAddress> |],
                    null)
            
            let defaultCtor =
                // this is the constructor with no parameters (default remote address)
                let args = []
                let c = ProvidedConstructor(args, (fun _ -> <@@ () @@>))
                let location = location
                let parentCtorCall (args: Expr list) = 
                    [ args.[0] 
                      <@@ BasicHttpBinding() @@>
                      <@@ EndpointAddress(location) @@> ]

                c.BaseConstructorCall <- (fun args -> parentCtor, parentCtorCall args )
                c
 
            let addressCtor =
                // this is the constructor with only the remote address
                let args = [ ProvidedParameter("remoteAddress", typeof<string>) ]
                let c = ProvidedConstructor(args, (fun _ -> <@@ () @@>))
                let parentCtorCall (args: Expr list) = 
                    [ args.[0]
                      <@@ BasicHttpBinding() @@>
                      <@@ EndpointAddress( %%(args.[1]) ) @@> ]

                c.BaseConstructorCall <- (fun args -> parentCtor, parentCtorCall args )

                c

            let fullCtor = 
                // this is the full constructor
                let args = 
                    [ ProvidedParameter("binding", typeof<System.ServiceModel.Channels.Binding>)
                      ProvidedParameter("remoteAddress", typeof<EndpointAddress>) ]
                let c = ProvidedConstructor( args, (fun _ -> <@@ () @@>))
                c.BaseConstructorCall <- (fun args -> parentCtor, args)
                c

            client.AddMembers([defaultCtor; addressCtor; fullCtor ])


    let defineLocation (client: ProvidedTypeDefinition) (location: string)=
    
        let location =
            ProvidedProperty("Location", typeof<string>, (fun _ -> Expr.Value(location) ), isStatic = true)

        client.AddMember(location)

    let defineOperationMethod (op: Operation,input, output) channel (itf: ProvidedTypeDefinition) (soapItf: ProvidedTypeDefinition) (client: ProvidedTypeDefinition)  =
        let name = op.Name

        // method on the soap interface including soap attributes
        let soapItfMeth = ProvidedMethod(name, input , output)
        soapItfMeth.AddCustomAttribute (mkOperationContractAttribute op.SoapAction "*")
        soapItfMeth.AddCustomAttribute(mkXmlSerializerFormatAttribute())
       
        soapItf.AddMember(soapItfMeth)

        // method on the user facing interface
        let itfMeth = ProvidedMethod(name, input , output)
        itf.AddMember(itfMeth)

        let code = 
            fun (args: Expr list) ->
                Expr.Call(channel args.[0], soapItfMeth, args.[1..])

        // method implementation
        let clientMeth = ProvidedMethod(name, input, output, code)

        client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(itf.FullName + "." + name, input , output, invokeCode = ( fun args -> Expr.Call(args.[0], clientMeth, args.[1..])))
        itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
        client.DefineMethodOverride(itfImpl, itfMeth)
        client.AddMember(itfImpl)

    let defineAsyncOperationMethod (op: Operation,input, output) channel (itf: ProvidedTypeDefinition) (soapItf: ProvidedTypeDefinition) (client: ProvidedTypeDefinition)  =
        let name = op.Name + "Async"

        // method on soap interface alwasy use tasks (include soap attributes)
        let taskOutput = ProvidedTypeBuilder.MakeGenericType(typedefof<Task<_>>, [ output ])
        let soapItfMeth = ProvidedMethod(name, input , taskOutput)
        soapItfMeth.AddCustomAttribute (mkOperationContractAttribute op.SoapAction "*")
        soapItfMeth.AddCustomAttribute(mkXmlSerializerFormatAttribute())

        soapItf.AddMember(soapItfMeth)

        // method on front facing interface using task
        let itfTaskMeth = ProvidedMethod(name, input , taskOutput)
        itf.AddMember(itfTaskMeth)

        // implementation of the task method
        let code = 
            fun (args: Expr list) ->
                Expr.Call(channel args.[0], soapItfMeth, args.[1..])

        let clientMeth = ProvidedMethod(name, input, taskOutput, code)

        client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(itf.FullName + "." + name, input , taskOutput, invokeCode = ( fun args -> Expr.Call(args.[0], clientMeth, args.[1..])))
        itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
        client.DefineMethodOverride(itfImpl, itfTaskMeth)
        client.AddMember(itfImpl)

        // method on front facing interface using Async
        let asyncName = "Async" + op.Name
        let asyncOutput = ProvidedTypeBuilder.MakeGenericType(typedefof<Async<_>>, [ output ])
        let itfAsyncMeth = ProvidedMethod(asyncName, input , asyncOutput)
        itf.AddMember(itfAsyncMeth)

        let code = 
            let awaitTaskGen = 
                typeof<Async>.GetMethods()
                |> Seq.find(fun m -> 
                    m.Name = "AwaitTask" 
                    && m.IsGenericMethod 
                    && (let ps = m.GetParameters() 
                        ps.Length = 1
                        && ps.[0].ParameterType.IsGenericType
                        && ps.[0].ParameterType.GetGenericTypeDefinition() = typedefof<Task<_>>) )

            let awaitTask = ProvidedTypeBuilder.MakeGenericMethod(awaitTaskGen, [output])

            let t = awaitTask.ReturnType.GetGenericArguments().[0]
            

            fun (args: Expr list) ->
                  Expr.Call(awaitTask, [ Expr.Call(channel args.[0], soapItfMeth, args.[1..] )])

        let clientMeth = ProvidedMethod(asyncName, input, asyncOutput, code)

        client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(itf.FullName + "." + asyncName, input , asyncOutput, invokeCode = ( fun args -> Expr.Call(args.[0], clientMeth, args.[1..])))
        itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
        client.DefineMethodOverride(itfImpl, itfAsyncMeth)
        client.AddMember(itfImpl)










    let buildWsdlTypes nsp (asm: ProvidedAssembly) name wsdl =
        let p = ProvidedTypeDefinition(asm, nsp, name, Some typeof<obj>, isSealed = false, isErased = false)
        asm.AddTypes([p])

        let types = Dictionary<string,ProvidedTypeDefinition>()


        let rec buildComplexType name (t: ComplexType) = 
            match types.TryGetValue(name) with
            | true, t -> Some (t :> Type)
            | _ ->
                let pt = ProvidedTypeDefinition(asm, nsp , name, Some typeof<obj>, isErased = false)
                pt.AddCustomAttribute(mkXmlTypeAttribute wsdl.TargetNamespace)

                types.Add(name, pt)

                let elements = 
                    [ for e in t.Elements do
                       let propType = typeRef e.Type
                          
                       e.Name, propType ]

                let fields =
                    [ for name, t in elements -> 
                        ProvidedField( String.camlCase name , t ) ]

                let props =
                    (elements,fields)
                    ||> List.mapi2 (fun i (name,t) field ->
                       let prop = ProvidedProperty(name, t, getterCode = (fun args -> Expr.FieldGet( args.[0], field) ), setterCode = (fun args -> Expr.FieldSet(args.[0], field, args.[1] ))) 
                       prop.AddCustomAttribute(mkXmlElementAttribute i)
                       prop
                    )


                let ctor = 
                    let ps =
                        [ for name, t in elements ->
                            ProvidedParameter(String.camlCase name, t) ]

                    ProvidedConstructor(ps, fun args -> 
                        let this = args.[0]
                        let sets = 
                            fields |> List.mapi (fun i field ->
                                Expr.FieldSet(this, field, args.[i+1] ))
                        List.fold (fun x y -> Expr.Sequential(x,y)) sets.[0] (List.tail sets)
                    )

                pt.AddMembers(fields)
                pt.AddMembers(props)
                pt.AddMember(ctor)
                pt.AddMember(ProvidedConstructor([], fun _ -> <@@ () @@>))

                Some (pt :> Type)


            
        and buildType = function
            | ComplexType { Elements = [ { MaxOccurs = Unbounded; Type = t } ]  } ->
                (typeRef t).MakeArrayType() |> Some
            | ComplexType t when 
                t.Name 
                |> Option.map (fun n -> n.StartsWith("ArrayOf")) 
                |> Option.defaultValue false  -> 
                    buildComplexType (Option.get t.Name ) t

            | ComplexType t ->
                buildComplexType (defaultArg t.Name "") t
            | _ -> None
        and typeRef t =
            match t with
            | Primitive XsdByte -> typeof<byte>
            | Primitive XsdShort -> typeof<int16>
            | Primitive XsdInt -> typeof<int>
            | Primitive XsdLong -> typeof<int64>
            | Primitive XsdDecimal -> typeof<decimal>
            | Primitive XsdDouble -> typeof<double>
            | Primitive XsdFloat -> typeof<float>
            | Primitive XsdString -> typeof<string>
            | Primitive XsdDate -> typeof<DateTime>
            | Primitive XsdDateTime -> typeof<DateTime>
            | Primitive XsdTime -> typeof<TimeSpan>
            | Primitive XsdBool -> typeof<bool>
            | Primitive XsdBase64Binary -> typeof<byte[]>
            | Primitive XsdHexBinary -> typeof<byte[]>
            | Element e ->
                failwithf "Cannot build top level element %s" e.Name
            | ComplexType { Elements = [ { MaxOccurs = Unbounded; Type = t} ] } ->
                (typeRef t).MakeArrayType()
            | ComplexType ct ->
                match buildComplexType (defaultArg ct.Name "") ct with
                | Some t -> t
                | None -> failwithf "Could not build Complex type %s" (defaultArg  ct.Name "(no name)") 
            | EmptyType _ -> typeof<obj>
            | TypeRef r -> 
                wsdl.Types
                |> List.tryPick (function ComplexType t when  t.Name =  Some r -> Some t | _ -> None)
                |> Option.bind(fun complexType -> buildType (*defaultArg complexType.Name "" *) (ComplexType complexType))
                |> function
                | Some t -> t
                | None -> failwithf "Could not build typeref %s" r
        let buildElement (e: Element) =
            match e.Type with
            | ComplexType { Elements = [ { MaxOccurs = Occurs 1; Type = t } ] } ->
                typeRef t
            | ComplexType { Elements = [ { MaxOccurs = Unbounded; Type = t } ] } ->
                (typeRef t).MakeArrayType()
            | ComplexType ct ->
                match buildComplexType e.Name  ct with
                | Some e -> 
                    e
                | _ -> failwithf "Cannot build element complextype %s" e.Name
                
            | _ -> failwithf "Canot build toplevel element %s" e.Name
            
            
        let buildPort serviceName (port: Port) = 
            try


            let soapItf = ProvidedTypeDefinition(asm, nsp, "I" + port.Name , None, isErased = false, isInterface = true)
            soapItf.AddCustomAttribute(mkServiceContractAttribute wsdl.TargetNamespace (nsp + "." + serviceName))

            let itf = ProvidedTypeDefinition(asm, nsp, port.Name , None, isErased = false, isInterface = true)

            let clientBase =
                let def = typedefof<System.ServiceModel.ClientBase<_>>
                ProvidedTypeBuilder.MakeGenericType(def, [ soapItf ])

            let client = 
                ProvidedTypeDefinition(
                        asm,
                        nsp,
                        port.Name + "Client",
                        Some clientBase,
                        isErased = false,
                        isSealed = false)

            defineCtors client port.Location
            defineLocation client port.Location

            client.AddInterfaceImplementation(itf)

            let channelProp = clientBase.GetProperty("Channel", BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetProperty)

            let channel this = 
                Expr.PropertyGet(this, channelProp)

            for op in port.Binding.Operations do
                let input = 
                    match op.Input.Type with
                    | ComplexType { Elements = [] } -> []
                    | ComplexType { Elements = [ { Name = name; MaxOccurs = Occurs 1; Type = t } ]} ->
                        [ ProvidedParameter(name, typeRef t) ]
                    | _ -> failwithf "Cannot parse input type %A" op.Input

                    
                let output = buildElement op.Output

                // synchronous method
                defineOperationMethod (op,input,output) channel itf soapItf client  

                // task method
                defineAsyncOperationMethod (op,input, output) channel itf soapItf client  

                ()

            
            [itf; soapItf ; client]
            with
            | ex -> failwithf "Failed while building ctor: %O" ex 

 
        let buildService (service : Service)  = 
            [ for port in service.Ports do
                yield! buildPort service.Name port ]

        let declaredTypes = 
            wsdl.Types
            |> List.choose buildType

        p.AddMembers(Seq.toList types.Values)

        p.AddMembers( wsdl.Services |> List.collect buildService)



        p

[<TypeProvider>]
type WsdlProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.WsdlProvider.DesignTime", "FSharp.Data.WsdlProvider.Runtime")])

    let ns = "FSharp.Data"
    let selfAsm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = selfAsm.GetName().Name)  


    let asm = ProvidedAssembly()
    let service = ProvidedTypeDefinition(selfAsm, ns, "WsdlProvider", Some typeof<obj>, isErased = false )

    do service.DefineStaticParameters(

        [ ProvidedStaticParameter("ServiceUri", typeof<string>) ],
        fun name args ->
            let uri = unbox<string> args.[0]
            let wsdl = 
                try
                    Wsdl.parse (System.Xml.Linq.XDocument.Load uri)
                with
                | ex -> failwithf "Error while loading wsdl:\n%O" ex

            try
                Provided.buildWsdlTypes ns asm name wsdl 
            with
            | ex -> failwithf "%O" ex 
          
        )

    do asm.AddTypes [ service ]

    do this.AddNamespace(
        ns, [service]
    )

[<TypeProviderAssembly>]
do ()
