module FSharp.Data.WsdlProviderImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.ServiceModel
open FSharp.Data.Wsdl
open FSharp.Data.ClientModel
open System.Threading.Tasks
open System.Xml.Linq
open System.Collections.Concurrent

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

    let mkXmlElementNameAttribute (name: XName, t: Type, order: int) =
        mkProvidedAttribute<XmlElementAttribute> [typeof<string>, box name.LocalName]
            [ "Type", box t
              "Order", box order
              if name.NamespaceName <> "" then
                "Namespace", box name.NamespaceName 
            ]

    let mkXmlAttributeAttribute (name: XName) =  
        mkProvidedAttribute<XmlAttributeAttribute> [typeof<string>, box name.LocalName] 
            [ if name.NamespaceName <> "" then
                "Namespace", box name.NamespaceName ]
    let mkXmlAttributeNameAttribute (name: XName, t: Type) =
        mkProvidedAttribute<XmlAttributeAttribute> [typeof<string>, box name.LocalName]
            [ "Type", box t
              if name.NamespaceName <> "" then
                "Namespace", box name.NamespaceName ]
    let mkXmlArrayAttribute (order: int) =  
        mkProvidedAttribute<XmlArrayAttribute> [] [ "Order", box order ]
    let mkXmlArrayItemAttribute (name: string, isNullable:bool) =  
        mkProvidedAttribute<XmlArrayItemAttribute> [typeof<string>, box name] 
                [ "IsNullable", box isNullable ]

    let mkXmlTypeAttribute (ns: string, anonymous) =
        mkProvidedAttribute<XmlTypeAttribute> []
            [ if anonymous then
                "AnonymousType", box true
              "Namespace", box ns]
  
    let mkServiceContractAttribute (ns: string,configName: string) =
        mkProvidedAttribute<ServiceContractAttribute> [] ["Namespace", box ns; "ConfigurationName", box configName ] 

    let mkMessageContractAttribute (name: XName,isWrapped: bool) =
        mkProvidedAttribute<MessageContractAttribute> [] 
            [ "WrapperName", box name.LocalName
              "WrapperNamespace", box name.NamespaceName
              "IsWrapped", box isWrapped ] 

    let mkMessageBodyMember (ns: string, order: int)  =
        mkProvidedAttribute<MessageBodyMemberAttribute> [] 
            [ "Namespace", box ns
              "Order", box order ]

    let mkXmlSerializerFormatAttribute() =
        mkProvidedAttribute<XmlSerializerFormatAttribute> [] [ "SupportFaults", box true]
    
    let mkXmlEnumAttribute (name: string) =
       mkProvidedAttribute<XmlEnumAttribute> [typeof<string>, box name] []
    
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
                let binding = DefaultBinding.SelectBinding(location)
                let parentCtorCall (args: Expr list) = 
                    [ args[0] // this
                      // the SelectBinding methods selects binding 
                      // matching location uri scheme
                      <@@ DefaultBinding.SelectBinding(location) @@>
                      <@@ EndpointAddress(location) @@> ]

                c.BaseConstructorCall <- (fun args -> parentCtor, parentCtorCall args )
                c
 
            let addressCtor =
                // this is the constructor with only the remote address
                let args = [ ProvidedParameter("remoteAddress", typeof<string>) ]
                let c = ProvidedConstructor(args, (fun _ -> <@@ () @@>))
                let parentCtorCall (args: Expr list) = 
                    [ args[0] // this
                      // the SelectBinding methods selects binding 
                      // matching location uri scheme
                      <@@ DefaultBinding.SelectBinding(%%(args[1])) @@>
                      <@@ EndpointAddress( %%(args[1]) ) @@> ]

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

    type ClientContext =
        { Channel: Expr -> Expr
          ClientInterface: ProvidedTypeDefinition
          SoapInterface: ProvidedTypeDefinition
          Client: ProvidedTypeDefinition }

    let rec getType (types: Dictionary<string, ProvidedTypeDefinition>) (tref: TRef) : Type =
        match tref with
        | TSimple t when t = typeof<unit> -> typeof<Void>
        | TSimple t -> t
        | TRef tr 
        | TEnum tr -> types[tr]
        | TRArray tr -> (getType types tr).MakeArrayType()
        | TRNullable tr -> typedefof<Nullable<_>>.MakeGenericType(getType types tr)



    let defineOperationMethod types  (op: OperationDef) clientCtx =
        let name = op.Name
        let outputType = getType types op.Output


        let input =
            [ for name, t in op.Input ->
                ProvidedParameter(name, getType types t ) ]

        // method on the soap interface including soap attributes
        let soapItfMeth = ProvidedMethod(name, input , outputType)
        soapItfMeth.AddCustomAttribute (mkOperationContractAttribute op.Action "*")
        soapItfMeth.AddCustomAttribute(mkXmlSerializerFormatAttribute())
       
        clientCtx.SoapInterface.AddMember(soapItfMeth)

        // method on the user facing interface
        let itfMeth = ProvidedMethod(name, input , outputType)
        clientCtx.ClientInterface.AddMember(itfMeth)

        let code = 
            fun (args: Expr list) ->
                Expr.Call(clientCtx.Channel args[0], soapItfMeth, args[1..])

        // method implementation
        let clientMeth = ProvidedMethod(name, input, outputType, code)

        clientCtx.Client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(clientCtx.ClientInterface.FullName + "." + name, input , outputType, invokeCode = ( fun args -> Expr.Call(args[0], clientMeth, args[1..])))
        itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
        clientCtx.Client.DefineMethodOverride(itfImpl, itfMeth)
        clientCtx.Client.AddMember(itfImpl)

    let defineAsyncOperationMethod types (op: OperationDef) clientCtx =
        let name = op.Name + "Async"

        // method on soap interface alwasy use tasks (include soap attributes)
        let output = getType types op.Output

        let taskOutput = 
            if output = typeof<Void> then
                typeof<Task>
            else
                ProvidedTypeBuilder.MakeGenericType(typedefof<Task<_>>, [ output ])

        let input =
            [ for name, t in op.Input ->
                ProvidedParameter(name, getType types t ) ]

        let soapItfMeth = ProvidedMethod(name, input , taskOutput)
        soapItfMeth.AddCustomAttribute (mkOperationContractAttribute op.Action "*")
        soapItfMeth.AddCustomAttribute(mkXmlSerializerFormatAttribute())

        clientCtx.SoapInterface.AddMember(soapItfMeth)

        // method on front facing interface using task
        let itfTaskMeth = ProvidedMethod(name, input , taskOutput)
        clientCtx.ClientInterface.AddMember(itfTaskMeth)

        // implementation of the task method
        let code = 
            fun (args: Expr list) ->
                Expr.Call(clientCtx.Channel args[0], soapItfMeth, args[1..])

        let clientMeth = ProvidedMethod(name, input, taskOutput, code)

        clientCtx.Client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(clientCtx.ClientInterface.FullName + "." + name, input , taskOutput, invokeCode = ( fun args -> Expr.Call(args[0], clientMeth, args[1..])))
        itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
        clientCtx.Client.DefineMethodOverride(itfImpl, itfTaskMeth)
        clientCtx.Client.AddMember(itfImpl)

        // method on front facing interface using Async
        let asyncName = "Async" + op.Name
        let asyncOutput = ProvidedTypeBuilder.MakeGenericType(typedefof<Async<_>>, [ output ])
        let itfAsyncMeth = ProvidedMethod(asyncName, input , asyncOutput)
        clientCtx.ClientInterface.AddMember(itfAsyncMeth)

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
                                && ps[0].ParameterType.IsGenericType
                                && ps[0].ParameterType.GetGenericTypeDefinition() = typedefof<Task<_>>) )
                    ProvidedTypeBuilder.MakeGenericMethod(awaitTaskGen, [output])

            fun (args: Expr list) ->
                Expr.Call(awaitTask, [ Expr.Call(clientCtx.Channel args[0], soapItfMeth, args[1..] ) ])

        let clientMeth = ProvidedMethod(asyncName, input, asyncOutput, code)

        clientCtx.Client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(clientCtx.ClientInterface.FullName + "." + asyncName, input , asyncOutput, invokeCode = ( fun args -> Expr.Call(args[0], clientMeth, args[1..])))
        itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
        clientCtx.Client.DefineMethodOverride(itfImpl, itfAsyncMeth)
        clientCtx.Client.AddMember(itfImpl)


    // builds a log2 deep Sequential tree from expression list
    // this is required for constructors that take many arguments.
    // The output sequence is used by a non-tail call function that will
    // raise stackoverflow if the nesting is too deep. Using a binary split
    // enables lower depth.
    let rec makeSequential (exprs: Expr list) =
        match exprs with
        | [] -> <@@ () @@>
        | [ e ] -> e
        | [ ex; ey] -> Expr.Sequential(ex,ey)
        | _ ->
            let left, right = List.splitAt(exprs.Length / 2) exprs
            Expr.Sequential( makeSequential left, makeSequential right)


    let buildWsdlTypes nsp (asm: ProvidedAssembly) name (wsdl: Wsdl) =

        let buildTypeDef (tdef: TypeDef) =
            let pt =
                match tdef with
                | Contract t ->
                    let pt = ProvidedTypeDefinition(asm, nsp, tdef.TypeName, Some typeof<obj>, isErased = false)
                    pt.AddCustomAttribute(mkMessageContractAttribute(t.XmlName, true))
                    pt
                | ComplexType t ->
                    let pt = ProvidedTypeDefinition(asm, nsp, tdef.TypeName, Some typeof<obj>, isErased = false)
                    pt.AddCustomAttribute(mkXmlTypeAttribute(t.XmlName.NamespaceName, false))
                    pt
                | AnonymousType t ->
                    let pt = ProvidedTypeDefinition(asm, nsp, tdef.TypeName, Some typeof<obj>, isErased = false)
                    pt.AddCustomAttribute(mkXmlTypeAttribute(t.XmlName.NamespaceName, true))
                    pt
                | NoNameType t -> 
                    let pt = ProvidedTypeDefinition(asm, nsp, tdef.TypeName, Some typeof<obj>, isErased = false)
                    pt 
                | EnumType e ->
                    let pt = ProvidedTypeDefinition(asm, e.XmlName.NamespaceName , e.TypeName, Some typeof<Enum>, isErased = false)
                    pt.AddCustomAttribute(mkXmlTypeAttribute(e.XmlName.NamespaceName,false))
                    pt
            tdef, pt


        let buildEnum (e: EnumTypeDef) (pt: ProvidedTypeDefinition) =
                pt.SetEnumUnderlyingType(typeof<int>)

                e.Values
                |> List.mapi (fun i e ->
                    let f = ProvidedField.Literal(e.Name, pt, box i)
                    f.AddCustomAttribute(mkXmlEnumAttribute e.Value)
                    f )
                |> pt.AddMembers

        let buildComplexType types (tdef: ComplexTypeDef) (pt: ProvidedTypeDefinition) =
            let fields = 
                [ for m in tdef.Members ->
                    let t = getType types m.TypeRef
                    ProvidedField(m.FieldName, t) ]
                

            let props = 
                [ for i,m in Seq.indexed tdef.Members do
                    let field = fields[i]
                    let t = field.FieldType
                    let prop = ProvidedProperty(m.PropName , t, getterCode = (fun args -> Expr.FieldGet( args[0], field) ), setterCode = (fun args -> Expr.FieldSet(args[0], field, args[1] ))) 
                    match m with
                    | CTElement(_, xsname,_, index)  -> 
                        prop.AddCustomAttribute(mkXmlElementAttribute index)
                    | CTContract(_, xsname,_, index)  -> 
                        prop.AddCustomAttribute(mkMessageBodyMember(xsname.NamespaceName , index))


                    | CTAttribute(_, xsname,_) -> prop.AddCustomAttribute(mkXmlAttributeAttribute xsname)
                    | CTArrayContract(_, xsname,_, itemName, index) ->
                        prop.AddCustomAttribute(mkMessageBodyMember(xsname.NamespaceName , index))
                        prop.AddCustomAttribute(mkXmlArrayItemAttribute(itemName, false))
                    | CTArray(_, xsname,_, itemName, index) ->
                        prop.AddCustomAttribute(mkXmlArrayAttribute index)
                        prop.AddCustomAttribute(mkXmlArrayItemAttribute(itemName, false))
                    | CTChoice (choices,i) ->
                        for c in choices do
                            match c with
                            | CTElement(_,xsname,t, _)
                            | CTContract(_,xsname,t, _) ->
                                let t = getType types t
                                prop.AddCustomAttribute(mkXmlElementNameAttribute(xsname,t,i))
                            | CTAttribute(_,xsname,t) ->
                                let t = getType types m.TypeRef
                                prop.AddCustomAttribute(mkXmlAttributeNameAttribute(xsname,t))
                            | CTArray _
                            | CTArrayContract _
                            | CTChoice _ -> ()
                            | CTArrayChoice _ -> ()
                            | CTAny _ -> ()
                    | CTArrayChoice(choices,i) ->
                        for c in choices do
                            match c with
                            | CTElement(_,xsname,t, _)
                            | CTContract(_,xsname,t, _) ->
                                let t = getType types t
                                prop.AddCustomAttribute(mkXmlElementNameAttribute(xsname,t,i))
                            | CTAttribute(_,xsname,t) ->
                                let t = getType types m.TypeRef
                                prop.AddCustomAttribute(mkXmlAttributeNameAttribute(xsname,t))
                            | CTArray _
                            | CTArrayContract _
                            | CTChoice _ -> ()
                            | CTArrayChoice _ -> ()
                            | CTAny -> ()
                    | CTAny -> ()

                            
                    prop ]




            let ctor = 
                match tdef.Members with
                | [] -> None // ctor is empty, and we will define one by default
                | _ ->
                    let ps =
                        [ for i,e in Seq.indexed tdef.Members do
                            match e with
                            | CTElement(name,_,_,_)
                            | CTContract(name,_,_,_)
                            | CTAttribute(name,_,_)
                            | CTArray(name,_,_,_,_) 
                            | CTArrayContract(name,_,_,_,_) ->
                                let field = fields[i]  
                                ProvidedParameter(String.camlCase name, field.FieldType)
                            | CTChoice _ ->
                                ProvidedParameter("item", typeof<obj>)
                            | CTArrayChoice _ ->
                                ProvidedParameter("items", typeof<obj[]>)
                            | CTAny _ ->
                                ProvidedParameter("item", typeof<obj>)
                                
                                ]

                    ProvidedConstructor(ps, fun args -> 
                        let this = args[0]
                        let sets = 
                            fields |> List.mapi (fun i field ->
                                Expr.FieldSet(this, field, args[i+1] ))

                        makeSequential sets ) |> Some

            pt.AddMembers(fields)
            pt.AddMembers(props)
            Option.iter pt.AddMember ctor
            pt.AddMember(ProvidedConstructor([], fun _ -> <@@ () @@>))

        let buildType types (tdef: TypeDef) (tp: ProvidedTypeDefinition) =
            match tdef with
            | Contract td
            | ComplexType td
            | AnonymousType td 
            | NoNameType td ->
                buildComplexType types td tp
            | EnumType e ->
                buildEnum e tp
            

        let buildOperation types (op: OperationDef) clientCtx =

            // synchronous method
            defineOperationMethod types op clientCtx

            // task method
            defineAsyncOperationMethod types op clientCtx


        let buildPort types serviceName (port: PortDef) = 
            try


            let soapItf = ProvidedTypeDefinition(asm, nsp, "I" + port.Name , None, isErased = false, isInterface = true)
            soapItf.AddCustomAttribute(mkServiceContractAttribute (port.Namespace, nsp + "." + serviceName))

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

            let clientCtx =
                { Channel = channel
                  ClientInterface = itf
                  SoapInterface = soapItf
                  Client = client }

            for op in port.Operations do
                buildOperation types op clientCtx

            
            [itf; soapItf ; client]
            with
            | ex -> failwithf "Failed while building ctor: %O" ex 

 
        let buildService types (service : ServiceDef)  = 
            [ for port in service.Ports do
                yield! buildPort types service.Name port ]


        let model = createModel wsdl
        let types =
            [ for tdef in model.Types do
                buildTypeDef tdef ]
        let typeNames = Dictionary<string,ProvidedTypeDefinition>()

        for _,t in types do
            typeNames.Add(t.Name, t)

        for tdef, t in types do
            buildType typeNames tdef t

        let services = model.Services |> List.collect (buildService typeNames)

        let p = ProvidedTypeDefinition(asm, nsp, name, Some typeof<obj>, isSealed = false, isErased = false)
        p.AddMembers(List.map snd types)
        p.AddMembers(services)

        asm.AddTypes([p])
        p


[<TypeProvider>]
type WsdlProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("FSharp.Data.WsdlProvider.DesignTime", "FSharp.Data.WsdlProvider.Runtime")], addDefaultProbingLocation=true)

    let ns = "FSharp.Data"
    let selfAsm = Assembly.GetExecutingAssembly()




    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DefaultBinding>.Assembly.GetName().Name = selfAsm.GetName().Name)  


    let service = ProvidedTypeDefinition(selfAsm, ns, "WsdlProvider", Some typeof<obj>, isErased = false )

    let cache = ConcurrentDictionary<string, string * ProvidedTypeDefinition>()

    do service.DefineStaticParameters(

        [ ProvidedStaticParameter("ServiceUri", typeof<string>)
          ProvidedStaticParameter("LocalSchemaFile", typeof<string>, "")
          ProvidedStaticParameter("ForceUpdate", typeof<bool>, false)
          ProvidedStaticParameter("ResolutionFolder", typeof<string>, "")],
        fun name args ->
            let uri = unbox<string> args[0]
            let localSchemaFile = unbox<string> args[1]
            let forceUpdate = unbox<bool> args[2]
            let resolutionFolder = unbox<string> args[3]

            match cache.TryGetValue(name) with
            | true, (existingUri, providedType)
                when existingUri = uri ->
                providedType
            | _ ->
                let wsdl = 
                    try
                        let basePath =
                            if String.IsNullOrEmpty resolutionFolder then
                                Environment.CurrentDirectory
                            elif resolutionFolder[resolutionFolder.Length-1] = Path.DirectorySeparatorChar then
                                resolutionFolder
                            else
                                resolutionFolder + string Path.DirectorySeparatorChar
                        let uri =
                            let u = Uri (uri, UriKind.RelativeOrAbsolute)
                            if u.IsAbsoluteUri then
                                u
                            else
                                Uri(Uri basePath, u)
                        if String.IsNullOrEmpty localSchemaFile then 
                            Wsdl.parse (System.Xml.Linq.XDocument.Load(string uri)) uri dontSave
                        else
                            let fullPath = 
                                if Path.IsPathRooted localSchemaFile then
                                    localSchemaFile
                                else
                                    Path.Combine(basePath, localSchemaFile)
                            if File.Exists fullPath && not forceUpdate then
                                
                                Wsdl.parseWsdlSchema (System.Xml.Linq.XDocument.Load fullPath) (Uri fullPath)
                            else
                                Wsdl.parse (System.Xml.Linq.XDocument.Load(string uri)) uri (saveLocalSchema fullPath)

                    with
                    | ex -> failwithf "Error while loading wsdl:\n%O" ex

                try
                    let asm = ProvidedAssembly()
                    let providedType = Provided.buildWsdlTypes service.Namespace asm name wsdl 

                    cache[name] <- (uri, providedType)
                    providedType
                with
                | ex -> failwithf "%O" ex 
        )


    do this.AddNamespace(
        ns, [service]
    )


[<TypeProviderAssembly>]
do ()
