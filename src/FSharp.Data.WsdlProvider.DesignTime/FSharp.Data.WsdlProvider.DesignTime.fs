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
open System.Xml.Linq
open System.Xml.Schema
open System.Collections.Concurrent

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
        let name = op.PortOperation.Name
        let outputType =
            if output = typeof<Unit> then
                typeof<Void>
            else
                output


        // method on the soap interface including soap attributes
        let soapItfMeth = ProvidedMethod(name, input , outputType)
        soapItfMeth.AddCustomAttribute (mkOperationContractAttribute op.SoapAction "*")
        soapItfMeth.AddCustomAttribute(mkXmlSerializerFormatAttribute())
       
        soapItf.AddMember(soapItfMeth)

        // method on the user facing interface
        let itfMeth = ProvidedMethod(name, input , outputType)
        itf.AddMember(itfMeth)

        let code = 
            fun (args: Expr list) ->
                Expr.Call(channel args.[0], soapItfMeth, args.[1..])

        // method implementation
        let clientMeth = ProvidedMethod(name, input, outputType, code)

        client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(itf.FullName + "." + name, input , outputType, invokeCode = ( fun args -> Expr.Call(args.[0], clientMeth, args.[1..])))
        itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
        client.DefineMethodOverride(itfImpl, itfMeth)
        client.AddMember(itfImpl)

    let defineAsyncOperationMethod (op: Operation,input, output) channel (itf: ProvidedTypeDefinition) (soapItf: ProvidedTypeDefinition) (client: ProvidedTypeDefinition)  =
        let name = op.PortOperation.Name + "Async"

        // method on soap interface alwasy use tasks (include soap attributes)
        let taskOutput = 
            if output = typeof<Unit> then
                typeof<Task>
            else
                ProvidedTypeBuilder.MakeGenericType(typedefof<Task<_>>, [ output ])
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
        let asyncName = "Async" + op.PortOperation.Name
        let asyncOutput = ProvidedTypeBuilder.MakeGenericType(typedefof<Async<_>>, [ output ])
        let itfAsyncMeth = ProvidedMethod(asyncName, input , asyncOutput)
        itf.AddMember(itfAsyncMeth)

        let code = 

            let awaitTask = 
                if output = typeof<Unit> then
                    typeof<Async>.GetMethod("AwaitTask", [| typeof<Task>|])
                else
                    let awaitTaskGen = 
                        typeof<Async>.GetMethods()
                        |> Seq.find(fun m -> 
                            m.Name = "AwaitTask" 
                            && m.IsGenericMethod 
                            && (let ps = m.GetParameters() 
                                ps.Length = 1
                                && ps.[0].ParameterType.IsGenericType
                                && ps.[0].ParameterType.GetGenericTypeDefinition() = typedefof<Task<_>>) )
                    ProvidedTypeBuilder.MakeGenericMethod(awaitTaskGen, [output])

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

        let types = Dictionary<XName,ProvidedTypeDefinition>()


        let rec buildComplexType (name: XName) (t: XmlSchemaComplexType) = 
            match types.TryGetValue(name) with
            | true, t -> t :> Type
            | false, _ ->
                let pt = ProvidedTypeDefinition(asm, nsp , name.LocalName, Some typeof<obj>, isErased = false)
                pt.AddCustomAttribute(mkXmlTypeAttribute name.NamespaceName)

                types.Add(name, pt)
                p.AddMember(pt)

                let elements = 
                    match t.Particle with
                    | XsdSequence elts ->
                        [ for e in elts do
                           let propType = typeRef e.ElementSchemaType
                              
                           e.Name, propType ]
                    | _ -> failwithf "Cannot build type with Particle of type %s" (t.Particle.GetType().FullName)

                if elements = [] then
                    failwith "Empty ComplexType"

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

                (pt :> Type)


            
        and typeRef (t: XmlSchemaType) =
            match t with
            | XsdSimpleType t ->
               t.Datatype.ValueType
            | XsdEmptyType t ->
                typeof<Unit>
            | XsdArray t  ->
                (typeRef t).MakeArrayType()
            | XsdComplexType ct ->
                buildComplexType ct.QualifiedName.XName ct
            | t -> failwithf "Unsupported schema type %s" (t.GetType().FullName)

        let buildElement (e: XmlSchemaElement) =
            match e.ElementSchemaType with
            | XsdArray t ->
                (typeRef t).MakeArrayType()
            | XsdComplexType (Particle (XsdSequence [ XsdElement e])) ->
                typeRef e.ElementSchemaType
            | XsdComplexType ct ->
                buildComplexType e.QualifiedName.XName ct
                
            | _ -> failwithf "Canot build toplevel element %O" e.Name
            
            
        let buildPort serviceName (port: Port) = 
            try


            let soapItf = ProvidedTypeDefinition(asm, nsp, "I" + port.Name.LocalName , None, isErased = false, isInterface = true)
            soapItf.AddCustomAttribute(mkServiceContractAttribute port.Name.NamespaceName (nsp + "." + serviceName))

            let itf = ProvidedTypeDefinition(asm, nsp, port.Name.LocalName , None, isErased = false, isInterface = true)

            let clientBase =
                let def = typedefof<System.ServiceModel.ClientBase<_>>
                ProvidedTypeBuilder.MakeGenericType(def, [ soapItf ])

            let client = 
                ProvidedTypeDefinition(
                        asm,
                        nsp,
                        port.Name.LocalName + "Client",
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
                    match op.PortOperation.Input with
                    | None 
                    | Some ({Element = Element (SchemaType XsdEmptyType)}) 
                        -> []
                    | Some ({ Element = Element e }) -> 
                        match e.ElementSchemaType with
                        | XsdComplexType (Particle (XsdSequence [XsdElement elt])) ->
                            [ ProvidedParameter(elt.Name, typeRef elt.ElementSchemaType) ]
                        | XsdComplexType t ->
                            [ ProvidedParameter(e.Name, buildComplexType e.QualifiedName.XName t ) ]
                        | XsdSimpleType t ->
                            [  ProvidedParameter(e.Name, t.Datatype.ValueType) ]
                        | t -> failwithf "Unsupported schema type %s" (t.GetType().FullName)

                    | Some ({ Name = n; Element = SimpleType t }) ->
                        [  ProvidedParameter(n, t.Datatype.ValueType) ]

                    
                let output = 
                    match op.PortOperation.Output with
                    | Some ({Element = Element (SchemaType XsdEmptyType)}) ->
                        typeof<Unit>
                    | Some { Element = Element e } -> buildElement e
                    | _ -> failwithf "Cannot prepare output for %s" op.PortOperation.Name
                    

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

        p.AddMembers( wsdl.Services |> List.collect buildService)

        p

[<TypeProvider>]
type WsdlProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.WsdlProvider.DesignTime", "FSharp.Data.WsdlProvider.Runtime")])

    let ns = "FSharp.Data"
    let selfAsm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = selfAsm.GetName().Name)  


    let service = ProvidedTypeDefinition(selfAsm, ns, "WsdlProvider", Some typeof<obj>, isErased = false )

    let cache = ConcurrentDictionary<string, string * ProvidedTypeDefinition>()

    do service.DefineStaticParameters(

        [ ProvidedStaticParameter("ServiceUri", typeof<string>) ],
        fun name args ->
            let uri = unbox<string> args.[0]

            match cache.TryGetValue(name) with
            | true, (existingUri, providedType)
                when existingUri = uri ->
                providedType
            | _ ->
                let wsdl = 
                    try
                        Wsdl.parse (System.Xml.Linq.XDocument.Load uri)
                    with
                    | ex -> failwithf "Error while loading wsdl:\n%O" ex

                try
                    let asm = ProvidedAssembly()
                    let providedType = Provided.buildWsdlTypes ns asm name wsdl 

                    cache.[name] <- (uri, providedType)
                    providedType
                with
                | ex -> failwithf "%O" ex 
        )


    do this.AddNamespace(
        ns, [service]
    )

[<TypeProviderAssembly>]
do ()
