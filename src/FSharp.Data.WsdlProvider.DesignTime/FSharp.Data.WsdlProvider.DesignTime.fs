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
open FSharp.Data.Xsd
open System.Threading.Tasks
open System.Xml.Linq
open System.Collections.Concurrent

module String =
    let camlCase (s: string) =
        if s.Length >= 1 then
            string (Char.ToLowerInvariant(s.[0])) + s.Substring(1);
        else
            s
    let PascalCase (s: string) =
        if s.Length >= 1 then
            string (Char.ToUpperInvariant(s.[0])) + s.Substring(1);
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
    let mkXmlAttributeAttribute (name: XName) =  
        mkProvidedAttribute<XmlAttributeAttribute> [typeof<string>, box name.LocalName] 
            [ if name.NamespaceName <> "" then
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
                    [ args.[0] // this
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
                    [ args.[0] // this
                      // the SelectBinding methods selects binding 
                      // matching location uri scheme
                      <@@ DefaultBinding.SelectBinding(%%(args.[1])) @@>
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

    type ClientContext =
        { Channel: Expr -> Expr
          ClientInterface: ProvidedTypeDefinition
          SoapInterface: ProvidedTypeDefinition
          Client: ProvidedTypeDefinition }

    let defineOperationMethod (op: Operation,input, output) clientCtx =
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
       
        clientCtx.SoapInterface.AddMember(soapItfMeth)

        // method on the user facing interface
        let itfMeth = ProvidedMethod(name, input , outputType)
        clientCtx.ClientInterface.AddMember(itfMeth)

        let code = 
            fun (args: Expr list) ->
                Expr.Call(clientCtx.Channel args.[0], soapItfMeth, args.[1..])

        // method implementation
        let clientMeth = ProvidedMethod(name, input, outputType, code)

        clientCtx.Client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(clientCtx.ClientInterface.FullName + "." + name, input , outputType, invokeCode = ( fun args -> Expr.Call(args.[0], clientMeth, args.[1..])))
        itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
        clientCtx.Client.DefineMethodOverride(itfImpl, itfMeth)
        clientCtx.Client.AddMember(itfImpl)

    let defineAsyncOperationMethod (op: Operation,input, output) clientCtx =
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

        clientCtx.SoapInterface.AddMember(soapItfMeth)

        // method on front facing interface using task
        let itfTaskMeth = ProvidedMethod(name, input , taskOutput)
        clientCtx.ClientInterface.AddMember(itfTaskMeth)

        // implementation of the task method
        let code = 
            fun (args: Expr list) ->
                Expr.Call(clientCtx.Channel args.[0], soapItfMeth, args.[1..])

        let clientMeth = ProvidedMethod(name, input, taskOutput, code)

        clientCtx.Client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(clientCtx.ClientInterface.FullName + "." + name, input , taskOutput, invokeCode = ( fun args -> Expr.Call(args.[0], clientMeth, args.[1..])))
        itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
        clientCtx.Client.DefineMethodOverride(itfImpl, itfTaskMeth)
        clientCtx.Client.AddMember(itfImpl)

        // method on front facing interface using Async
        let asyncName = "Async" + op.PortOperation.Name
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
                                && ps.[0].ParameterType.IsGenericType
                                && ps.[0].ParameterType.GetGenericTypeDefinition() = typedefof<Task<_>>) )
                    ProvidedTypeBuilder.MakeGenericMethod(awaitTaskGen, [output])

            fun (args: Expr list) ->
                  Expr.Call(awaitTask, [ Expr.Call(clientCtx.Channel args.[0], soapItfMeth, args.[1..] )])

        let clientMeth = ProvidedMethod(asyncName, input, asyncOutput, code)

        clientCtx.Client.AddMember(clientMeth)
        let itfImpl = ProvidedMethod(clientCtx.ClientInterface.FullName + "." + asyncName, input , asyncOutput, invokeCode = ( fun args -> Expr.Call(args.[0], clientMeth, args.[1..])))
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


    type CTChildKind =
        | CTElement
        | CTArray of string
        | CTAttribute


    type CTXmlName =
        | XmlType of XName
        | Anonymous of XName
        | NoName

    let buildWsdlTypes nsp (asm: ProvidedAssembly) name wsdl =
        let p = ProvidedTypeDefinition(asm, nsp, name, Some typeof<obj>, isSealed = false, isErased = false)
        asm.AddTypes([p])

        let types = Dictionary<XName,ProvidedTypeDefinition>()
        let typeNames = Dictionary<string,int>()

        // avoid name clashes by adding a number suffix to existing type names
        let fixTypeName name =
            match typeNames.TryGetValue(name) with
            | false, _ ->
                typeNames.[name] <- 0
                name
            | true, n ->
                let newN = n + 1
                typeNames.[name] <- newN
                name + string newN

        let rec buildComplexType contract name (xmlname: CTXmlName) (t: XsComplexType) = 
            match xmlname with
            | XmlType n 
            | Anonymous n ->
                match types.TryGetValue(n) with
                | true, t -> t :> Type |> Some
                | false, _ -> None

            | _ -> None
            |> function
               | Some t -> t
               | None ->
            
                let typeName = fixTypeName name 
                let pt = ProvidedTypeDefinition(asm, nsp , typeName, Some typeof<obj>, isErased = false)
                match xmlname with
                | XmlType n ->
                    if contract then
                        pt.AddCustomAttribute(mkMessageContractAttribute(n, true))
                    else
                        pt.AddCustomAttribute(mkXmlTypeAttribute(n.NamespaceName, false))
                    types.Add(n, pt)
                | Anonymous n ->
                    pt.AddCustomAttribute(mkXmlTypeAttribute(n.NamespaceName, true))
                | NoName -> ()

                p.AddMember(pt)


                let elements = 
                    match t.Elements with
                    | Sequence elts ->
                        [ for p in elts do
                            match p with
                            | XsElement ({ Type = TypeRef t } as e) 
                            | XsElement ({ Type = InlineType (XsSimpleType { BaseType = t }) } as e) ->
                                
                                let innerType = 
                                    if Schema.isBuiltInSimpleType t then
                                        None
                                    else
                                        Some wsdl.Schemas.Types.[t]

                                match innerType with
                                | Some { Type = XsComplexType { Attributes = []; Elements = Sequence [ XsElement { Name = itemName; Occurs = { Max = max }; Type = TypeRef titem } ]}} when max > MaxOccurs 1 ->
                                    // This is actually an array
                                    let ref : Type = typeRef titem
                                    let propType = ref.MakeArrayType()
                                    yield (e.Name.LocalName, e.Name, propType, CTArray itemName.LocalName)
                                | Some { Name = arrayName ; Type = XsComplexType { Attributes = []; Elements = Sequence [ XsElement { Name = itemName; Occurs = { Max = max }; Type = InlineType (XsComplexType ct) } ]}} when max > MaxOccurs 1 ->
                                
                                    let pt = buildComplexType false (String.PascalCase arrayName.LocalName + String.PascalCase itemName.LocalName) (Anonymous arrayName) ct
                                    let propType = pt.MakeArrayType()
                                    yield (e.Name.LocalName, e.Name, propType, CTArray itemName.LocalName)

                                | _ ->

                                    let propType = 
                                        if e.Occurs.Max > MaxOccurs 1 then
                                            let ref : Type = typeRef t
                                            ref.MakeArrayType()
                                        else
                                            typeRef t
                                    yield (e.Name.LocalName, e.Name, propType, CTElement)

                                     
                                      
                            | XsElement ( { Type = InlineType (XsComplexType ct)} as e) ->
                                let propType = 
                                    let pt = buildComplexType false (name + String.PascalCase e.Name.LocalName) NoName ct
                                    if e.Occurs.Max > MaxOccurs 1 then
                                        pt.MakeArrayType()
                                    else
                                        pt
                                    

                                yield (e.Name.LocalName, e.Name, propType, CTElement)
                            | XsAny _ -> () ]
                    | _ -> []

                let elementNames = set [ for (n,_,_,_) in elements -> n ]
                let fixAttributeName name  = 
                    if Set.contains name elementNames then
                        name + "Attribute"
                    else
                        name

                let attributes =
                    [ for a in t.Attributes do
                        let attrType = attributeTypeRef a.Type
                        let name = fixAttributeName a.Name.LocalName

                        name, a.Name, attrType, CTAttribute ]

                let all = elements @ attributes

                if not (List.isEmpty all) then

                    let fields =
                        [ for name, _, t, _ in all -> 
                            ProvidedField( String.camlCase name, t ) ]

                    let props =
                        (all,fields)
                        ||> List.mapi2 (fun i (name, xsname,t, kind) field ->
                           let prop = ProvidedProperty(name, t, getterCode = (fun args -> Expr.FieldGet( args.[0], field) ), setterCode = (fun args -> Expr.FieldSet(args.[0], field, args.[1] ))) 
                           match kind with
                           | CTElement -> 
                                if contract then
                                    prop.AddCustomAttribute(mkMessageBodyMember(xsname.NamespaceName , i))
                                else
                                    prop.AddCustomAttribute(mkXmlElementAttribute i)
                           | CTAttribute -> prop.AddCustomAttribute(mkXmlAttributeAttribute xsname)
                           | CTArray itemName ->
                                if contract then
                                    prop.AddCustomAttribute(mkMessageBodyMember(xsname.NamespaceName , i))
                                else
                                    prop.AddCustomAttribute(mkXmlArrayAttribute i)
                                prop.AddCustomAttribute(mkXmlArrayItemAttribute(itemName, false))
                                
                           prop
                        )


                    let ctor = 
                        let ps =
                            [ for name,_, t,_ in all ->
                                ProvidedParameter(String.camlCase name, t) ]

                        ProvidedConstructor(ps, fun args -> 
                            let this = args.[0]
                            let sets = 
                                fields |> List.mapi (fun i field ->
                                    Expr.FieldSet(this, field, args.[i+1] ))

                            makeSequential sets
                            
                        )

                    pt.AddMembers(fields)
                    pt.AddMembers(props)
                    pt.AddMember(ctor)
                pt.AddMember(ProvidedConstructor([], fun _ -> <@@ () @@>))

                (pt :> Type)
        and buildEnum (name: XName) (t: XsSimpleType) =
            match types.TryGetValue(name) with
            | true, t -> t :> Type
            | false, _ ->
                let typeName = fixTypeName name.LocalName

                let pt = ProvidedTypeDefinition(asm, nsp , typeName, Some typeof<Enum>, isErased = false)
                pt.SetEnumUnderlyingType(typeof<int>)
                pt.AddCustomAttribute(mkXmlTypeAttribute(name.NamespaceName,false))

                types.Add(name, pt)
                p.AddMember(pt)

                t.Enumeration
                |> List.mapi (fun i e ->
                    let f = ProvidedField.Literal(e.Name, pt, box i)
                    f.AddCustomAttribute(mkXmlEnumAttribute e.Value)
                    f )
                |> pt.AddMembers

                pt :> Type


        and buildType contract (typeDef: XsTypeDef) =
            match typeDef.Type with
            | XsComplexType t ->
                buildComplexType contract typeDef.Name.LocalName (XmlType typeDef.Name) t
            | XsSimpleType ({ Enumeration = _ :: _ } as t)  ->
                buildEnum typeDef.Name t

            | XsSimpleType t ->
                match  Schema.builtInSimpleType t.BaseType with
                | Some ty -> ty
                | None -> failwith "Unsupported simple type base type"
            
        and typeRef (name: XName) =
            match Schema.builtInSimpleType name with
            | Some t -> t
            | None  ->
                match types.TryGetValue(name) with
                | true, t -> t :> Type
                | false,_ ->                    
                    buildType false wsdl.Schemas.Types.[name]


        and attributeTypeRef (t: XsAttributeType) =
            match t with
            | XsSimple t -> typeRef t
            | XsList t -> (typeRef t).MakeArrayType()

        let buildMessage contract (name: XName) (typeRef: XsTypeRef) =
            match typeRef with
            | TypeRef name 
            | InlineType (XsSimpleType { BaseType = name}) ->
                match Schema.builtInSimpleType name with
                | Some t -> t
                | None  ->
                    match types.TryGetValue(name) with
                    | true, t -> t :> Type
                    | false,_ ->                    
                        buildType contract wsdl.Schemas.Types.[name]
            | InlineType (XsComplexType t) ->
                match types.TryGetValue(name) with
                | true, t -> t :> Type
                | false,_ ->                    
                    buildComplexType contract name.LocalName (XmlType name) t
                

 
        let buildParameters contract (name: XName) (t: XsTypeRef) =
            match t with
            | InlineType (XsComplexType { Elements = NoContent; BaseType = None}) ->
                // no content, this is a void method
                []
            | InlineType (XsComplexType { Elements = Sequence [ XsElement ({ Type = tn} as elt) ]}) ->
                // No need for all this wrapping. Just use the element/type inside
                [ ProvidedParameter(elt.Name.LocalName, buildMessage contract name tn) ]
            | tn ->
                [ ProvidedParameter(name.LocalName, buildMessage contract name tn ) ]

        let buildElement contract (e: XsElement) =
            match e with
            | { Type = t   ; Occurs = { Max = Unbounded }} ->
                (buildMessage contract  e.Name t).MakeArrayType()
            | { Type = InlineType (XsComplexType { Elements = Sequence [ XsElement { Type = TypeRef t} ] })} when not contract  ->
                buildMessage contract e.Name (TypeRef t)
            | { Type = InlineType (XsSimpleType { BaseType = t}) } when not contract ->
                buildMessage contract e.Name (TypeRef t)
            | { Type = InlineType (XsComplexType { BaseType = None; Elements = NoContent; Mixed = false; Attributes = []; AnyAttribute = false}) } ->
                typeof<unit>
            | { Type = ct } ->
                buildMessage contract e.Name ct
                
            | _ -> failwithf "Canot build toplevel element %O" e.Name
            
        let buildOperation op clientCtx =
            let contract = op.PortOperation.RequireContract
            let input = 
                match op.PortOperation.Input with
                | None  -> []
                | Some {Element = Element { Name = n; Type = t }} -> 
                    buildParameters contract n t
                | Some { Name = n; Element = SimpleType t } ->
                    buildParameters contract (XName.Get n) (TypeRef t)

            let output = 
                match op.PortOperation.Output with
                | Some {Element = Element e } ->
                    buildElement contract e 
                | Some {Element = SimpleType t } ->
                    typeRef t
                | None ->
                    typeof<unit>

            // synchronous method
            defineOperationMethod (op,input,output) clientCtx

            // task method
            defineAsyncOperationMethod (op,input, output) clientCtx

            ()

        let buildPort serviceName (port: Port) = 
            try


            let soapItf = ProvidedTypeDefinition(asm, nsp, "I" + port.Name.LocalName , None, isErased = false, isInterface = true)
            soapItf.AddCustomAttribute(mkServiceContractAttribute (port.Name.NamespaceName, nsp + "." + serviceName))

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

            let clientCtx =
                { Channel = channel
                  ClientInterface = itf
                  SoapInterface = soapItf
                  Client = client }

            for op in port.Binding.Operations do
                buildOperation op clientCtx

            
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
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("FSharp.Data.WsdlProvider.DesignTime", "FSharp.Data.WsdlProvider.Runtime")], addDefaultProbingLocation=true)

    let ns = "FSharp.Data"
    let selfAsm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DefaultBinding>.Assembly.GetName().Name = selfAsm.GetName().Name)  


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
