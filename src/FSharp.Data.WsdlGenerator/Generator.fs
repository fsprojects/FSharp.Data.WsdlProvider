namespace FSharp.Data.WsdlGenerator

open System
open System.Text
open FSharp.Data
open FSharp.Data.Wsdl
open FSharp.Data.ClientModel
open Myriad.Core
open FSharp.Compiler
open System.ServiceModel
open System.ServiceModel.Channels
open System.Xml.Linq


type Builder =
    { Indentation: int
      Writer: StringBuilder }

    member this.Indent() =
        { this with Indentation = this.Indentation+1 }

    member this.Unindent() =
        { this with Indentation = this.Indentation-1 }

    member this.StartIndent(indent) =
        this.Writer.Append(' ', (this.Indentation+indent) * 4) |> ignore
        this

    member this.StartIndent() =
        this.StartIndent(0)

    member this.StartLine(indent, text: string) =
        this.StartIndent(indent).Writer.AppendLine(text) |> ignore
        this

    member this.StartLine(text) =
        this.StartLine(0, text)

    member this.StartAppend(indent, text: string) =
        this.StartIndent(indent).Writer.Append(text) |> ignore
        this

    member this.StartAppend(text: string) =
        this.StartAppend(0, text)

    member this.Append(text: string) =
        this.Writer.Append(text) |> ignore
        this

    member this.AppendJoin(separator: string,items: string seq) =
        this.Writer.AppendJoin(separator, items) |> ignore
        this

    member this.AppendLine() =
        this.Writer.AppendLine() |> ignore
        this

    member this.AppendLine(text: string) =
        this.Writer.AppendLine(text) |> ignore
        this

    member this.Fold(f: 'a -> Builder -> Builder, items) =
        Seq.fold (fun builder item  -> f item builder) this items

    member this.Apply(f: Builder -> Builder) =
        f this

    override this.ToString() =
        this.Writer.ToString()
        
module Builder =
    let fold f items (builder: Builder) =
        builder.Fold(f, items)

    let newLine (builder: Builder) =
        builder.AppendLine()


        
type DefaultBinding = 
    static member SelectBinding(uri: string) =
        if (Uri uri).Scheme = Uri.UriSchemeHttps then
            BasicHttpsBinding() :> Binding
        else
            BasicHttpBinding() :> Binding

module Generation = 
    open System.Xml.Serialization

    let attributeSuffix = "Attribute"
    let fsharpCorePrefix = "Microsoft.FSharp.Core."


    let cst (arg: obj) =
        match arg with
        | :? bool as v -> if v then "true" else "false"
        | :? int as v -> string v
        | :? uint as v -> string v
        | :? byte as v -> string v
        | :? sbyte as v -> string v
        | :? int16 as v -> string v
        | :? uint16 as v -> string v
        | :? int64 as v -> string v
        | :? uint64 as v -> string v
        | :? string as v -> "\"" + v + "\""
        | :? Type as v -> $"typeof<{v.FullName}>"
        | :? TRef as v -> $"typeof<{v.FsName}>"
        | _ -> failwith $"Unable to convert attribute value of type {arg.GetType().FullName} to string"

    let cleanAttribute (name: string ) =
        let cleanSuffix (name: string) = 
            if name.EndsWith attributeSuffix then
                name.Substring(0, name.Length - attributeSuffix.Length)
            else
                name
        let cleanNamespace (name: string) =
            if name.StartsWith fsharpCorePrefix then
                name.Substring(fsharpCorePrefix.Length)
            else
                name

        name
        |> cleanSuffix
        |> cleanNamespace

    let cleanId (id: string) =
        match id with
        | "type"
        | "for"
        | "member"
        | "exception"
        | "end"
        | "class"
        | "default"
        | "constraint" -> $"``{id}``"
        | _ -> id

    let mkdAttribute<'t>(args : (Type*obj) list) (namedArgs: (string * obj) list) (builder: Builder)  = 
        let t = typeof<'t>
        let attrArgs = 
            seq { for _,arg in args do 
                    cst arg
                  for (name, arg) in namedArgs do
                    $"{name} = {cst arg}" }
        builder.StartAppend($"[<{cleanAttribute t.FullName}(")
                .AppendJoin(", ", attrArgs)
                .AppendLine(")>]")
                
    let mkAllowNullLiteralAttribute =
        mkdAttribute<AllowNullLiteralAttribute> [] [] 

    let mkSerializableAttribute =
        mkdAttribute<SerializableAttribute> [] [] 

    let mkDefaultValueAttribute =
        mkdAttribute<DefaultValueAttribute> [] [] 

    let mkXmlIgnoreAttribute =
        mkdAttribute<XmlIgnoreAttribute> [] [] 

    let mkXmlElementAttribute (order: int) =  
        mkdAttribute<XmlElementAttribute> [] [ "Order", box order ]

    let mkXmlElementNameAttribute (name: XName, t: TRef, order: int) =
        mkdAttribute<XmlElementAttribute> [typeof<string>, box name.LocalName]
            [ "Order", box order
              "Type", box t
              if name.NamespaceName <> "" then
                "Namespace", box name.NamespaceName 
            ]

    let mkXmlAttributeAttribute (name: XName) =  
        mkdAttribute<XmlAttributeAttribute> [typeof<string>, box name.LocalName] 
            [ if name.NamespaceName <> "" then
                "Namespace", box name.NamespaceName ]
    let mkXmlAttributeNameAttribute (name: XName, t: TRef) =
        mkdAttribute<XmlAttributeAttribute> [typeof<string>, box name.LocalName]
            [ "Type", box t
              if name.NamespaceName <> "" then
                "Namespace", box name.NamespaceName ]
    let mkXmlArrayAttribute (order: int) =  
        mkdAttribute<XmlArrayAttribute> [] [ "Order", box order ]
    let mkXmlArrayItemAttribute (name: string, isNullable:bool) =  
        mkdAttribute<XmlArrayItemAttribute> [typeof<string>, box name] 
                [ "IsNullable", box isNullable ]

    let mkXmlTypeAttribute (ns: string, anonymous) =
        mkdAttribute<XmlTypeAttribute> []
            [ if anonymous then
                "AnonymousType", box true
              "Namespace", box ns]
  
    let mkServiceContractAttribute (ns: string,configName: string) =
        mkdAttribute<ServiceContractAttribute> [] ["Namespace", box ns; "ConfigurationName", box configName ] 

    let mkMessageContractAttribute (name: XName,isWrapped: bool) =
        mkdAttribute<MessageContractAttribute> [] 
            [ "WrapperName", box name.LocalName
              "WrapperNamespace", box name.NamespaceName
              "IsWrapped", box isWrapped ] 

    let mkMessageBodyMember (ns: string, order: int)  =
        mkdAttribute<MessageBodyMemberAttribute> [] 
            [ "Namespace", box ns
              "Order", box order ]

    let mkXmlSerializerFormatAttribute =
        mkdAttribute<XmlSerializerFormatAttribute> [] [ "SupportFaults", box true]
    
    let mkXmlEnumAttribute (name: string) =
       mkdAttribute<XmlEnumAttribute> [typeof<string>, box name] []
    
    let mkOperationContractAttribute (action: string) (replyAction: string) =
        mkdAttribute<OperationContractAttribute> [] [ "Action", box action; "ReplyAction", box replyAction]
    

    let emptyCtor name (location: string) (builder: Builder) =
        let args = 
            let binding = DefaultBinding.SelectBinding(location)
            [ $"{binding.GetType().FullName}()"
              $"{typeof<System.ServiceModel.EndpointAddress>.FullName} {cst location}" ]

        builder.StartLine($"new() =")
               .Indent()
                   .StartAppend($"new {name}(")
                   .AppendJoin(", ", args)
                   .AppendLine(")")
               .Unindent()
               .AppendLine()

    let addressCtor name (builder: Builder) =
        let args = 
            [ $"DefaultBinding.SelectBinding(address)"
              $"{typeof<System.ServiceModel.EndpointAddress>.FullName} address" ]

        builder.StartLine("new(address: string) = ")
                .Indent()
                    .StartAppend($"new {name}(")
                    .AppendJoin(", ", args)
                    .AppendLine(")")
                .Unindent()
                .AppendLine()


    let defineCtors name (location: string) (builder: Builder) =
        builder
        |> emptyCtor name location 
        |> addressCtor name 
 


    let defineLocation (location: string) (builder: Builder) =
        builder.StartLine($"static member Location = {cst location}")
            .AppendLine()
        

    let taskType (tref: TRef) =
        match tref with
        | TSimple t when t = typeof<unit> -> "Task"
        | n -> $"Task<{n.FsName}>"

    let defineOperationMethod (op: OperationDef) (builder: Builder) =
        let name = op.Name
        builder.StartAppend($"member this.{name}(")
               .AppendJoin(", ", [ for name, t in op.Input -> $"{cleanId name}: {t.FsName}"])
               .AppendLine($") : {op.Output.FsName} =")
               .Indent()
                   .StartAppend($"base.Channel.{name}(")
                   .AppendJoin(", ", [ for name,_ in op.Input -> cleanId name ])
                   .AppendLine(")")

               .Unindent()


    let defineAsyncOperationMethod (op: OperationDef) (builder: Builder) =
        let name = op.Name + "Async"
        builder.StartAppend($"member this.{name}(")
               .AppendJoin(", ", [ for name, t in op.Input -> $"{cleanId name}: {t.FsName}"])
               .AppendLine($") : {taskType op.Output} =")
               .Indent()
                   .StartAppend($"base.Channel.{name}(")
                   .AppendJoin(", ", [ for name,_ in op.Input -> cleanId name ])
                   .AppendLine(")")
               .Unindent()

    let defineSoapItfOperationMethod (op: OperationDef) (builder: Builder) =
        let name = op.Name
        match op.Input with
        | [] ->
            builder.Indent()
                    .Apply(mkOperationContractAttribute op.Action "*")
                    .Apply(mkXmlSerializerFormatAttribute)
                   .StartAppend($"abstract {name}: unit -> {op.Output.FsName}")
                   .Unindent()

        | _ ->

            builder.Indent()
                   .Apply(mkOperationContractAttribute op.Action "*")
                   .Apply(mkXmlSerializerFormatAttribute)
                   .StartAppend($"abstract {name}: ")
                   .AppendJoin(" * ", [ for name, t in op.Input -> $"{cleanId name}: {t.FsName}"])
                   .AppendLine($" -> {op.Output.FsName}")
                   .Unindent()


    let defineSoapItfAsyncOperationMethod (op: OperationDef) (builder: Builder) =
        let name = op.Name + "Async"
        match op.Input with
        | [] ->
            builder.Indent()
                   .Apply(mkOperationContractAttribute op.Action "*")
                   .Apply(mkXmlSerializerFormatAttribute)
                   .StartAppend($"abstract {name}: unit -> {taskType op.Output}")
                   .Unindent()


        | _ ->
            builder.Indent()
                   .Apply(mkOperationContractAttribute op.Action "*")
                   .Apply(mkXmlSerializerFormatAttribute)
                   .StartAppend($"abstract {name}: ")
                   .AppendJoin(", ", [ for name, t in op.Input -> $"{cleanId name}: {t.FsName}"])
                   .AppendLine($" -> {taskType op.Output}")
                   .Unindent()


    let buildWsdlTypes nsp wsdl =
        let rec buildProp (m: CTChild) (builder: Builder) =
            let mkAttribute m =
                match m with
                | CTElement(_, _, _, i)  -> 
                        mkXmlElementAttribute i
                | CTContract(_, xsname, _, i)  -> 
                        mkMessageBodyMember(xsname.NamespaceName , i)
                | CTAttribute(_, xsname,_,_) -> mkXmlAttributeAttribute xsname
                | CTArray(_, _,_, itemName, i) ->
                        mkXmlArrayAttribute i
                        >> mkXmlArrayItemAttribute(itemName, false)
                | CTArrayContract(_, xsname,_, itemName, i) ->
                    mkMessageBodyMember(xsname.NamespaceName , i)
                    >> mkXmlArrayItemAttribute(itemName, false)

                | CTChoice (choices,i) ->
                        
                    Builder.fold (fun c -> 
                        match c with
                        | CTElement(_,xsname,t, i)
                        | CTContract(_,xsname,t, i) ->
                            mkXmlElementNameAttribute(xsname,t,i)
                        | CTAttribute(_,xsname,t,_) ->
                            mkXmlAttributeNameAttribute(xsname,t)
                        | CTArray _
                        | CTArrayContract _
                        | CTChoice _ -> id
                        | CTArrayChoice _ -> id
                        | CTAny -> id
                    )  choices

                | CTArrayChoice(choices,i) ->
                    Builder.fold (fun c -> 
                        match c with
                        | CTElement(_,xsname,t, _)
                        | CTContract(_,xsname,t, _) ->
                            mkXmlElementNameAttribute(xsname,t,i)
                        | CTAttribute(_,xsname,t,_) ->
                            mkXmlAttributeNameAttribute(xsname,t)
                        | CTArray _
                        | CTArrayContract _
                        | CTChoice _ -> id
                        | CTArrayChoice _ -> id
                        | CTAny-> id
                    )  choices
                | CTAny -> id

            if m.RequireSpecifiedField then
                builder.Apply(mkAttribute m).StartLine($"member this.{cleanId m.PropName} with get() = this.{m.FieldName} and set v = this.{m.FieldName} <- v; this.{m.SpecifiedFieldName} <- true")
                    .Apply(mkXmlIgnoreAttribute).StartLine($"member this.{cleanId m.SpecifiedPropName } = this.{m.SpecifiedFieldName}")

            else
                builder.Apply(mkAttribute m).StartLine($"member this.{cleanId m.PropName} with get() = this.{m.FieldName} and set v = this.{m.FieldName} <- v")

        let rec buildField (m: CTChild) (builder: Builder) =

            let  b = builder.Apply(mkDefaultValueAttribute).StartLine($"val mutable private {m.FieldName}: { m.FsTypeName }")
            if m.RequireSpecifiedField then
                b.Apply(mkDefaultValueAttribute).StartLine($"val mutable private {m.SpecifiedFieldName}: bool")
            else
                b



        and buildComplexType (t: ComplexTypeDef) (builder: Builder) =
            match t.Members with
            | [] ->
                builder.Apply(mkSerializableAttribute)
                        .StartLine($"type {t.TypeName}() =")
                        .Indent()
                        .StartLine("class")
                        .StartLine("end")
                        .Unindent()
                        .AppendLine()

            | _ ->
                builder.Apply(mkSerializableAttribute)
                        .StartLine($"type {t.TypeName}() =")
                        .Indent()
                            .Fold(buildField, t.Members)
                            .AppendLine()
                            .Fold(buildProp, t.Members)
                            .StartAppend($"new(")
                               .AppendJoin(", ", [ for  m in t.Members -> m.FieldName ])
                            
                            .AppendLine(") as this =")
                            .Indent()
                                .StartLine($"{t.TypeName}() then")
                                .Indent()
                                .Fold( ( fun (m: CTChild) b -> b.StartLine($"this.{m.FieldName} <- {m.FieldName}")), t.Members )
                                .Unindent()
                            .Unindent()
                        .Unindent()
                        .AppendLine()


        and buildEnum (t: EnumTypeDef) (builder: Builder) =

            builder.StartLine($"type {t.TypeName} =")
                    .Indent()
                        .Fold((fun (e: EnumValue) builder -> 
                             builder.StartLine("| ")
                                    .Indent()
                                    .Apply(mkXmlEnumAttribute e.Value)
                                    .StartLine(1, $"{e.Name} = {e.Index}")
                                    .Unindent()
                           ), t.Values)
                    .Unindent()
                


        and buildType (typeDef: TypeDef) (builder: Builder) : Builder =
            match typeDef with
            | Contract t ->
                builder
                |> mkAllowNullLiteralAttribute
                |> mkMessageContractAttribute(t.XmlName , true)
                |> buildComplexType t
            | ComplexType t ->
                builder
                |> mkAllowNullLiteralAttribute
                |> mkXmlTypeAttribute(t.XmlName.NamespaceName, false)
                |> buildComplexType t
            | AnonymousType t -> 
                builder
                |> mkAllowNullLiteralAttribute
                |> mkXmlTypeAttribute(t.XmlName.NamespaceName, true)
                |> buildComplexType t
            | NoNameType t -> 
                builder
                |> mkAllowNullLiteralAttribute
                |> buildComplexType t
            | EnumType t ->
                builder
                |> mkXmlTypeAttribute(t.XmlName.NamespaceName,false)
                |> buildEnum t

           

        let buildOperation (op: OperationDef) (builder: Builder) =

            // synchronous method
            builder
            |> defineOperationMethod op
            // task method
            |> defineAsyncOperationMethod op

        let buildSoapItfOperation (op: OperationDef) (builder: Builder) =
            builder
            |> defineSoapItfOperationMethod op
            |> defineSoapItfAsyncOperationMethod op
           
        let mkInterface name (builder: Builder) =
            
            builder
            |> mkdAttribute<InterfaceAttribute> [] []
            |> fun builder ->
                builder.StartLine($"type {name} =")




        let buildPort serviceName (port: PortDef) (builder: Builder) = 
            let soapItfName = "I" + port.Name


            let declareType (builder: Builder) =
                let args = 
                    [ $"binding: {typeof<Binding>.FullName}"
                      $"address: {typeof<EndpointAddress>.FullName}"]
                builder.StartAppend($"type {serviceName}(")
                    .AppendJoin(", ", args)
                    .AppendLine(") =")
                    .Indent()
                    .StartLine($"inherit System.ServiceModel.ClientBase<{soapItfName}>(binding, address)")



            builder
            |> mkServiceContractAttribute (port.Namespace, nsp + "." + serviceName)
            |> mkInterface soapItfName 
            |> Builder.fold buildSoapItfOperation port.Operations
            |> Builder.newLine
            |> mkInterface port.Name
            |> Builder.fold buildSoapItfOperation port.Operations
            |> Builder.newLine
            |> declareType
            |> defineCtors serviceName port.Location
            |> defineLocation port.Location
            |> Builder.fold buildOperation port.Operations
            

        let selectBinding (builder: Builder) =
            builder.StartLine("type DefaultBinding = ")
                    .StartLine(1, "static member SelectBinding(uri: string) =")
                    .StartLine(2, "if (Uri uri).Scheme = Uri.UriSchemeHttps then")
                    .StartLine(3, $"{typenameof<BasicHttpsBinding>}() :> {typenameof<Binding>}")
                    .StartLine(2, "else")
                    .StartLine(3, $"{typenameof<BasicHttpBinding>}() :> {typenameof<Binding>}")
                    .AppendLine()
            
            
        
 
        let buildService (service : ServiceDef) (builder: Builder)  = 
            builder
            |> selectBinding
            |> Builder.fold (buildPort service.Name) service.Ports


        let model = createModel wsdl


        { Indentation = 0; Writer = StringBuilder()}
            .StartLine($"namespace rec {nsp}")
            .AppendLine()
            .StartLine("open System")
            .StartLine("open System.Threading.Tasks")
            .AppendLine()
            .Fold((fun (td: TypeDef) -> buildType td) , model.Types)
            .Fold(buildService, model.Services)
            .ToString()

[<MyriadGenerator("wsdl")>]
type WsdlGenerator() =
    let dontSave _ _ = () 

    let loadWsdl inputFile uri =
        try
            let basePath =
            
                let inputPath = IO.Path.GetDirectoryName(inputFile: string) + "/"
                if IO.Path.IsPathRooted(inputPath) then
                    inputPath
                else
                    IO.Path.Combine(IO.Directory.GetCurrentDirectory(), inputPath)

            let uri =
                let u = Uri (uri, UriKind.RelativeOrAbsolute)
                if u.IsAbsoluteUri then
                    u
                else
                    Uri(Uri basePath, u)
            Wsdl.parse (System.Xml.Linq.XDocument.Load(string uri)) uri dontSave

        with
        | ex -> failwithf "Error while loading wsdl:\n%O" ex



    let getConfig key defaultValue (config: (string * obj) seq) =
            config |> Seq.tryPick (fun (n, v) -> if n = key then Some (v :?> string) else None  )
                    |> Option.defaultValue defaultValue

    interface IMyriadGenerator with
        member _.Generate(ctx: GeneratorContext): Output = 
            
            let configKey = ctx.ConfigKey |> Option.defaultValue "none"
            
            let config = ctx.ConfigGetter configKey 
            let ns = getConfig "namespace" "Wsdl" config
            let uri = getConfig "uri" "unknown uri" config

            let wsdl = loadWsdl ctx.InputFilename uri

            let source = Generation.buildWsdlTypes ns wsdl
            let output =
                try
                    Fantomas.Core.CodeFormatter.FormatDocumentAsync(false, source, Fantomas.Core.FormatConfig.FormatConfig.Default )  |> Async.RunSynchronously
                with
                | ex ->
                    printfn "%O" ex
                    source
            Output.Source(output)


        member _.ValidInputExtensions: seq<string> = 
            [ ".fs" ]
