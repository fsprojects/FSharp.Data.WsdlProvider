namespace FSharp.Data.WsdlGenerator

open System
open System.Text
open FSharp.Data
open FSharp.Data.Wsdl
open Myriad.Core
open FSharp.Compiler
open System.ServiceModel
open System.Collections.Generic
open System.ServiceModel.Channels
open FSharp.Data.Xsd
open System.Xml.Linq

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
module ClientModel =
    type TRef =
        | TRef of TRType
        | TRArray of TRef
        | TRNullable of TRef
        with
        member this.IsStruct =
            match this with
            | TRef tr -> tr.Struct
            | TRArray _ -> false
            | TRNullable _ -> true

        member this.MakeArrayType() =
            TRArray this

        member this.MakeNullableType() =
            if this.IsStruct then
                TRNullable this
            else
                this

        member this.Name =
            match this with
            | TRef t -> t.Name
            | TRArray t -> t.Name + "[]"
            | TRNullable t -> $"Nullable<{t.Name}>"

        static member Unit = TRef { Name = "unit"; Struct = false }

    and TRType=
        { Name: string 
          Struct: bool }


    type CTChild =
        | CTElement of string * XName * TRef
        | CTContract of string * XName * TRef
        | CTAttribute of string * XName * TRef
        | CTArray of string * XName * TRef * string
        | CTArrayContract of string * XName * TRef * string
        | CTChoice of CTChild list
        with
            member this.FieldName =
                match this with
                | CTElement(name, _,_)
                | CTContract(name, _,_)
                | CTAttribute(name, _,_)
                | CTArray(name, _,_,_)
                | CTArrayContract(name, _,_,_) ->
                     String.camlCase name
                | CTChoice _ -> "item"

            member this.PropName =
                match this with
                | CTElement(name, _,_)
                | CTContract(name, _,_)
                | CTAttribute(name, _,_)
                | CTArray(name, _,_,_)
                | CTArrayContract(name, _,_,_) ->
                    String.PascalCase name
                | CTChoice _ -> "Item"
            member this.TypeName =
                match this with
                | CTElement(_, _,t)
                | CTContract(_, _,t)
                | CTAttribute(_, _,t)
                | CTArray(_, _,t,_)
                | CTArrayContract(_, _,t,_) ->
                    t.Name 
                | CTChoice _ -> "obj"

    type CTXmlName =
        | XmlType of XName
        | Anonymous of XName
        | NoName

    type ComplexTypeDef =
        { TypeName: string
          XmlName: XName
          Members: CTChild list }


    type EnumTypeDef =
        { TypeName: string
          XmlName: XName
          SimpleType: XsSimpleType }

    type TypeDef =
        | Contract of ComplexTypeDef
        | ComplexType of ComplexTypeDef
        | AnonymousType of ComplexTypeDef
        | NoNameType of ComplexTypeDef
        | EnumType of EnumTypeDef

        member this.TypeName =
            match this with
            | Contract t 
            | ComplexType t
            | AnonymousType t -> t.TypeName
            | NoNameType t -> t.TypeName
            | EnumType t -> t.TypeName

    type OperationDef =
        { Name: string
          Input: (string * TRef) list
          Output: TRef }

    type PortDef =
        { Name: string
          Namespace: XNamespace
          Location: string
          Operations: OperationDef list }
    type ServiceDef =
        { Name: string
          Ports: PortDef list }

    type WsdlDef =
        { Types: TypeDef list
          Services: ServiceDef list }

    let simpleTypeRef (t:Type) =
        let str name = TRef { Name = name; Struct = t.IsValueType }
        match t.FullName with
        | "System.Boolean" -> str "bool"
        | "System.Int32" -> str "int"
        | "System.UInt32" -> str "uint"
        | "System.Int16" -> str "int16"
        | "System.UInt16" -> str "uint16"
        | "System.Int64" -> str "int64"
        | "System.UInt64" -> str "uint64"
        | "System.String" -> str "string"
        | n -> TRef { Name = n; Struct = t.IsValueType }

    let flattenWsdl wsdl =
        let typeRefs = Dictionary<XName, TRef>()
        let typeNames = Dictionary<string,int>()
        let types = ResizeArray<TypeDef>()


        //// avoid name clashes by adding a number suffix to existing type names
        let fixTypeName name =
            match typeNames.TryGetValue(name) with
            | false, _ ->
                typeNames.[name] <- 0
                name
            | true, n ->
                let newN = n + 1
                typeNames.[name] <- newN
                name + string newN


        let rec flattenComplexType contract name (xmlname: CTXmlName) (t: XsComplexType) = 
            printfn $"Flatten {name} / {xmlname}"
            match xmlname with
            | XmlType n 
            | Anonymous n ->
                match typeRefs.TryGetValue(n) with
                | true, t -> t |> Some
                | false, _ -> None

            | _ -> None
            |> function
               | Some t -> t
               | None ->
            
                let typeName = fixTypeName name
                let tr = TRef { Name = typeName; Struct = false }
                match xmlname with
                | XmlType n ->
                    typeRefs.Add(n, tr)
                | Anonymous n ->
                    typeRefs.Add(n, tr)
                | NoName -> ()



                let elements = 
                    match t.Elements with
                    | Sequence elts ->
                        [ for p in elts do

                            let rec getCT p =
                              [ match p with
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
                                        let ref = typeRef titem
                                        let propType = ref.MakeArrayType()
                                        let md = (e.Name.LocalName, e.Name, propType, itemName.LocalName)
                                        if contract then
                                            CTArrayContract md
                                        else
                                            CTArray md
                                    | Some { Name = arrayName ; Type = XsComplexType { Attributes = []; Elements = Sequence [ XsElement { Name = itemName; Occurs = { Max = max }; Type = InlineType (XsComplexType ct) } ]}} when max > MaxOccurs 1 ->
                                    
                                        let pt = flattenComplexType false (String.PascalCase arrayName.LocalName + String.PascalCase itemName.LocalName) (Anonymous arrayName) ct
                                        let propType = pt.MakeArrayType()
                                        CTArray (e.Name.LocalName, e.Name, propType, itemName.LocalName)

                                    | _ ->

                                        let propType = applyElementCardinality e (typeRef t) 
                                        let md = (e.Name.LocalName, e.Name, propType)
                                        if contract then
                                            CTContract md
                                        else
                                            CTElement md

                                         
                                          
                                | XsElement ( { Type = InlineType (XsComplexType ct)} as e) ->
                                    let propType = 
                                        let pt = flattenComplexType false (name + String.PascalCase e.Name.LocalName) NoName ct
                                        applyElementCardinality e pt
                                        
                                    let md = (e.Name.LocalName, e.Name, propType)
                                    if contract then
                                        CTContract md
                                    else
                                        CTElement md
                                | XsAny _ -> ()
                                | XsChoice choices ->
                                    CTChoice (choices.Items |> List.collect getCT)
                                
                                ]

                            yield! getCT p
                               
                            ]
                    | _ -> []



                let elementNames = set [ for e in elements ->
                                            match e with
                                            | CTElement(n,_,_)
                                            | CTContract(n,_,_)
                                            | CTAttribute(n,_,_)
                                            | CTArray(n,_,_,_)
                                            | CTArrayContract(n,_,_,_) -> n
                                            | CTChoice _ -> "Item"
                                            ]
                let fixAttributeName name  = 
                    if Set.contains name elementNames then
                        name + "Attribute"
                    else
                        name

                let attributes =
                    [ for a in t.Attributes do
                        let attrType = attributeTypeRef a.Type
                        let name = fixAttributeName a.Name.LocalName

                        CTAttribute(name, a.Name, attrType) ]

                let all = elements @ attributes
                match
                    all
                    |> Seq.countBy(id)
                    |> Seq.filter(fun (_,c) -> c>1)
                    |> Seq.toList with
                | [] -> ()
                | l -> failwithf "Duplicate elements %s" (l |> Seq.map (sprintf "%A") |> String.concat ",")

                let td = 
                    match xmlname with
                    | XmlType n ->
                        if contract then
                            Contract { TypeName = typeName; XmlName = n; Members = all  } 
                        else
                            ComplexType { TypeName = typeName; XmlName = n; Members = all  } 
                    | Anonymous n ->
                        AnonymousType { TypeName = typeName ; XmlName = n; Members = all }
                    | NoName -> 
                        NoNameType { TypeName = typeName; XmlName = null; Members = all}
                types.Add(td)


                tr


        and flattenType contract (typeDef: XsTypeDef) =
            match typeDef.Type with
            | XsComplexType t ->
                flattenComplexType contract typeDef.Name.LocalName (XmlType typeDef.Name) t
            | XsSimpleType ({ Enumeration = _ :: _ } as t)  ->
                flattenEnum typeDef.Name t

            | XsSimpleType t ->
                match  Schema.builtInSimpleType t.BaseType with
                | Some ty -> simpleTypeRef ty
                | None -> failwith "Unsupported simple type base type"


        and flattenEnum (name: XName) (t: XsSimpleType) =
            match typeRefs.TryGetValue(name) with
            | true, t -> t
            | false, _ ->
                let typeName = fixTypeName name.LocalName

                let td = EnumType { TypeName = typeName; XmlName = name; SimpleType = t }
                types.Add(td)
                let tr = TRef { Name = typeName; Struct = true }
                typeRefs.Add(name, tr )
                tr

 
        and applyElementCardinality e (ref: TRef) =
            if e.Occurs.Max > MaxOccurs 1 then
                ref.MakeArrayType()
            elif e.Nillable then
                ref.MakeNullableType()
            else
                ref

        and typeRef (name: XName) : TRef =
            match Schema.builtInSimpleType name with
            | Some t -> simpleTypeRef t
            | None  ->
                match typeRefs.TryGetValue(name) with
                | true, t -> t
                | false,_ -> flattenType false wsdl.Schemas.Types.[name]



        and attributeTypeRef (t: XsAttributeType) =
            match t with
            | XsSimple t -> typeRef t
            | XsList t -> (typeRef t).MakeArrayType()
 
 

        let flattenMessage contract (name: XName) (typeRef: XsTypeRef) =
            if not(typeRefs.ContainsKey name) then
                match typeRef with
                | TypeRef btname 
                | InlineType (XsSimpleType { BaseType = btname}) ->
                    match Schema.builtInSimpleType btname with
                    | Some t ->
                        typeRefs.Add(name, simpleTypeRef t)
                    | None  ->
                        match typeRefs.TryGetValue(btname) with
                        | true, t -> 
                            typeRefs.Add(name, t)
                        | false,_ ->                    
                            flattenType contract wsdl.Schemas.Types.[name] |> ignore
                | InlineType (XsComplexType t) ->
                    match typeRefs.TryGetValue(name) with
                    | true, t -> ()
                    | false,_ ->                    
                        flattenComplexType contract name.LocalName (XmlType name) t |> ignore

        let flattenElement contract (e: XsElement) =
            match e with
            | { Type = t   ; Occurs = { Max = Unbounded }} ->
                flattenMessage contract  e.Name t
            | { Type = InlineType (XsComplexType { Elements = Sequence [ XsElement { Type = TypeRef t} ] })} when not contract  ->
                flattenMessage contract e.Name (TypeRef t)
            | { Type = InlineType (XsSimpleType { BaseType = t}) } when not contract ->
                flattenMessage contract e.Name (TypeRef t)
            | { Type = InlineType (XsComplexType { BaseType = None; Elements = NoContent; Mixed = false; Attributes = []; AnyAttribute = false}) } ->
                let name = e.Name
                let typeName = fixTypeName name.LocalName
                types.Add(TypeDef.ComplexType { TypeName = typeName; XmlName = name; Members = []})
                typeRefs.Add(name, TRef { Name = typeName; Struct = false })
                
            | { Type = ct } ->
                flattenMessage contract e.Name ct

        let flattenParameters contract (name: XName) (t: XsTypeRef) =
            match t with
            | InlineType (XsComplexType { Elements = NoContent; BaseType = None}) ->
                // no content, this is a void method
                ()
            | InlineType (XsComplexType { Elements = Sequence [ XsElement ({ Type = tn} as elt) ]}) ->
                // No need for all this wrapping. Just use the element/type inside
                flattenMessage contract name tn 
            | tn ->
                flattenMessage contract name tn 


        let flattenOperation op =
            let contract = op.PortOperation.RequireContract
            match op.PortOperation.Input with
            | None  -> ()
            | Some {Element = Element { Name = n; Type = t }} -> 
                flattenParameters contract n t
            | Some { Name = n; Element = SimpleType t } ->
                flattenParameters contract (XName.Get n) (TypeRef t)

            match op.PortOperation.Output with
            | Some {Element = Element e } ->
                printfn $"Flatten Op output Elt {e.Name}"
                flattenElement contract e 
            | Some {Element = SimpleType t } ->
                printfn $"Flatten Op output ST {t}"
                typeRef t |> ignore
            | None ->
                ()

        let flattenPort (port: Port) =
            for op in port.Binding.Operations do
               flattenOperation op 

        let flattenService (service: Service) =
                for port in service.Ports do
                    flattenPort port 

        for service in wsdl.Services do
            flattenService service


        types, typeRefs

    let createModel wsdl =

        let types, typeRefs = flattenWsdl wsdl


        let findTypeRef (name: XName) : TRef =
            match Schema.builtInSimpleType name with
            | Some t -> simpleTypeRef t
            | None  ->
                match typeRefs.TryGetValue(name) with
                | true, t -> t
                | false,_ -> failwith $"TypeRef {name} not found"

        let buildParameters (name: XName) (t: XsTypeRef) =
            match t with
            | InlineType (XsComplexType { Elements = NoContent; BaseType = None}) ->
                // no content, this is a void method
                []
            | InlineType (XsComplexType { Elements = Sequence [ XsElement ({ Type = TypeRef tn} as elt) ]}) ->
                // No need for all this wrapping. Just use the element/type inside

                [ String.camlCase elt.Name.LocalName, findTypeRef tn ]
            | tn ->
                [ String.camlCase name.LocalName, findTypeRef name ]


        let buildOp (op: Operation) =
            let input = 
                match op.PortOperation.Input with
                | None  -> []
                | Some {Element = Element { Name = n; Type = t }} -> 
                    buildParameters n t
                | Some { Name = n; Element = SimpleType t } ->
                    buildParameters (XName.Get n) (TypeRef t)

            let output = 
                match op.PortOperation.Output with
                | Some {Element = Element e } ->
                    findTypeRef e.Name
                | Some {Element = SimpleType t } ->
                    findTypeRef t
                | None ->
                    TRef.Unit 
            { Name = op.PortOperation.Name
              Input = input
              Output = output }

        let buildPort (port: Port)  = 
            { Name = port.Name.LocalName
              Namespace = port.Name.Namespace
              Location = port.Location
              Operations = [ for op in port.Binding.Operations -> buildOp op ]
            }




        let buildService (service: Service) =
            { Name = service.Name
              Ports = [ for port in service.Ports -> buildPort port]
              }

        let services =
            [ for service in wsdl.Services do
                buildService service ]


        { Types = Seq.toList types
          Services = services }



[<AutoOpen>]
module Operators =
    let inline typenameof<'t> = typeof<'t>.FullName


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

    member this.Foldi(f: int -> 'a -> Builder -> Builder, items) =
        Seq.fold (fun (builder, i) item  -> f i item builder,i+1) (this,0) items |> fst

    member this.Apply(f: 'a -> Builder -> Builder , item) =
        f item this

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
    let fshaprCorePrefix = "Microsoft.FSharp.Core."


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
        | :? ClientModel.TRef as v -> $"typeof<{v.Name}>"
        | _ -> failwith $"Unable to convert attribute value of type {arg.GetType().FullName} to string"

    let cleanAttribute (name: string ) =
        let cleanSuffix (name: string) = 
            if name.EndsWith attributeSuffix then
                name.Substring(0, name.Length - attributeSuffix.Length)
            else
                name
        let cleanNamespace (name: string) =
            if name.StartsWith fshaprCorePrefix then
                name.Substring(fshaprCorePrefix.Length)
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
        | "constraint" -> $"``{id}``"
        | _ -> id

    let mkdAttribute<'t>(args : (Type*obj) list) (namedArgs: (string * obj) list) (builder: Builder)  = 
        let t = typeof<'t>
        let attrArgs = 
            seq { for t,arg in args do 
                    cst arg
                  for (name, arg) in namedArgs do
                    $"{name} = {cst arg}" }
        builder.StartAppend($"[<{cleanAttribute t.FullName}(")
                .AppendJoin(", ", attrArgs)
                .AppendLine(")>]")
                

    let mkXmlElementAttribute (order: int) =  
        mkdAttribute<XmlElementAttribute> [] [ "Order", box order ]

    let mkXmlElementNameAttribute (name: XName, t: ClientModel.TRef) =
        mkdAttribute<XmlElementAttribute> [typeof<string>, box name.LocalName]
            [ "Type", box t
              if name.NamespaceName <> "" then
                "Namespace", box name.NamespaceName 
            ]

    let mkXmlAttributeAttribute (name: XName) =  
        mkdAttribute<XmlAttributeAttribute> [typeof<string>, box name.LocalName] 
            [ if name.NamespaceName <> "" then
                "Namespace", box name.NamespaceName ]
    let mkXmlAttributeNameAttribute (name: XName, t: ClientModel.TRef) =
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

    let mkXmlSerializerFormatAttribute() =
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
        

    let taskType (tref: ClientModel.TRef) =
        match tref.Name with
        | "unit" -> "Task"
        | n -> $"Task<{n}>"

    let defineOperationMethod (op: ClientModel.OperationDef) (builder: Builder) =
        let name = op.Name
        builder.StartAppend($"member this.{name}(")
               .AppendJoin(", ", [ for name, t in op.Input -> $"{cleanId name}: {t.Name}"])
               .AppendLine($") : {op.Output.Name} =")
               .Indent()
                   .StartAppend($"base.Channel.{name}(")
                   .AppendJoin(", ", [ for name,_ in op.Input -> cleanId name ])
                   .AppendLine(")")

               .Unindent()


    let defineAsyncOperationMethod (op: ClientModel.OperationDef) (builder: Builder) =
        let name = op.Name + "Async"
        builder.StartAppend($"member this.{name}(")
               .AppendJoin(", ", [ for name, t in op.Input -> $"{cleanId name}: {t.Name}"])
               .AppendLine($") : {taskType op.Output} =")
               .Indent()
                   .StartAppend($"base.Channel.{name}(")
                   .AppendJoin(", ", [ for name,_ in op.Input -> cleanId name ])
                   .AppendLine(")")

               .Unindent()

    let defineSoapItfOperationMethod (op: ClientModel.OperationDef) (builder: Builder) =
        let name = op.Name
        match op.Input with
        | [] ->
            builder.Indent()
                   .StartAppend($"abstract {name}: unit -> {op.Output.Name}")
                   .Unindent()

        | _ ->

            builder.Indent()
                   .StartAppend($"abstract {name}: ")
                   .AppendJoin("* ", [ for name, t in op.Input -> $"{t.Name}"])
                   .AppendLine($" -> {op.Output.Name}")
                   .Unindent()


    let defineSoapItfAsyncOperationMethod (op: ClientModel.OperationDef) (builder: Builder) =
        let name = op.Name + "Async"
        match op.Input with
        | [] ->
            builder.Indent()
                   .StartAppend($"abstract {name}: unit -> {taskType op.Output}")
                   .Unindent()


        | _ ->
            builder.Indent()
                   .StartAppend($"abstract {name}: ")
                   .AppendJoin(", ", [ for name, t in op.Input -> $"{t.Name}"])
                   .AppendLine($" -> {taskType op.Output}")
                   .Unindent()


    let buildWsdlTypes nsp wsdl =
        let rec buildMember i (m: ClientModel.CTChild) (builder: Builder) =
            let mkAttribute m =
                match m with
                | ClientModel.CTElement(_, _ ,_)  -> 
                        mkXmlElementAttribute i
                | ClientModel.CTContract(_, xsname,_)  -> 
                        mkMessageBodyMember(xsname.NamespaceName , i)
                | ClientModel.CTAttribute(_, xsname,_) -> mkXmlAttributeAttribute xsname
                | ClientModel.CTArray(_, _,_, itemName) ->
                        mkXmlArrayAttribute i
                        >> mkXmlArrayItemAttribute(itemName, false)
                | ClientModel.CTArrayContract(_, xsname,_, itemName) ->
                    mkMessageBodyMember(xsname.NamespaceName , i)
                    >> mkXmlArrayItemAttribute(itemName, false)

                | ClientModel.CTChoice choices ->
                        
                    Builder.fold (fun c -> 
                        match c with
                        | ClientModel.CTElement(_,xsname,t)
                        | ClientModel.CTContract(_,xsname,t) ->
                            mkXmlElementNameAttribute(xsname,t)
                        | ClientModel.CTAttribute(_,xsname,t) ->
                            mkXmlAttributeNameAttribute(xsname,t)
                        | ClientModel.CTArray _
                        | ClientModel.CTArrayContract _
                        | ClientModel.CTChoice _ -> id
                    )  choices

            builder.Apply(mkAttribute, m).StartLine($"member val {m.PropName} : { m.TypeName } = {cleanId m.FieldName} with get, set")

        and buildComplexType (t: ClientModel.ComplexTypeDef) (builder: Builder) =
            match t.Members with
            | [] ->
                builder.StartLine($"type {t.TypeName}() =")
                        .Indent()
                        .StartLine("class")
                        .StartLine("end")
                        .Unindent()
                        .AppendLine()

            | _ ->
                builder.StartLine($"type {t.TypeName}(")
                        .AppendJoin(", ", [ for  m in t.Members -> cleanId m.FieldName ])
                        .AppendLine(") =")
                        .Indent()
                        .Foldi(buildMember, t.Members)
                        .Unindent()
                        .AppendLine()


        and buildEnum (t: ClientModel.EnumTypeDef) (builder: Builder) =

            builder.StartLine($"type {t.TypeName} =")
                    .Indent()
                        .Foldi((fun i e builder -> 
                             builder.StartLine("| ")
                                    .Indent()
                                    .Apply(mkXmlEnumAttribute, e.Value)
                                    .StartLine(1, $"{e.Name} = {i}")
                                    .Unindent()
                           ), t.SimpleType.Enumeration)
                    .Unindent()
                


        and buildType (typeDef: ClientModel.TypeDef) (builder: Builder) : Builder =
            match typeDef with
            | ClientModel.Contract t ->
                builder
                |> mkMessageContractAttribute(t.XmlName , true)
                |> buildComplexType t
            | ClientModel.ComplexType t ->
                builder
                |> mkXmlTypeAttribute(t.XmlName.NamespaceName, false)
                |> buildComplexType t
            | ClientModel.AnonymousType t -> 
                builder
                |> mkXmlTypeAttribute(t.XmlName.NamespaceName, true)
                |> buildComplexType t
            | ClientModel.NoNameType t -> 
                builder
                |> buildComplexType t
            | ClientModel.EnumType t ->
                builder
                |> mkXmlTypeAttribute(t.XmlName.NamespaceName,false)
                |> buildEnum t

           

        let buildOperation (op: ClientModel.OperationDef) (builder: Builder) =

            // synchronous method
            builder
            |> defineOperationMethod op
            // task method
            |> defineAsyncOperationMethod op

        let buildSoapItfOperation (op: ClientModel.OperationDef) (builder: Builder) =
            builder
            |> defineSoapItfOperationMethod op
            |> defineSoapItfAsyncOperationMethod op
           
        let mkInterface name (builder: Builder) =
            
            builder
            |> mkdAttribute<InterfaceAttribute> [] []
            |> fun builder ->
                builder.StartLine($"type {name} =")




        let buildPort serviceName (port: ClientModel.PortDef) (builder: Builder) = 
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
            |> mkServiceContractAttribute (port.Namespace.NamespaceName, nsp + "." + serviceName)
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
            
            
        
 
        let buildService (service : ClientModel.ServiceDef) (builder: Builder)  = 
            builder
            |> selectBinding
            |> Builder.fold (buildPort service.Name) service.Ports


        let model = ClientModel.createModel wsdl


        { Indentation = 0; Writer = StringBuilder()}
            .StartLine($"namespace rec {nsp}")
            .AppendLine()
            .StartLine("open System")
            .StartLine("open System.Threading.Tasks")
            .AppendLine()
            .Fold((fun (td: ClientModel.TypeDef) -> buildType td) , model.Types)
            .Fold(buildService, model.Services)
            .ToString()

[<MyriadGenerator("wsdl")>]
type WsdlGenerator() =
    let dontSave _ _ = () 

    let loadWsdl inputFile uri =
        try
            let basePath = IO.Path.GetDirectoryName(inputFile: string) + "/"
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
            let name = getConfig "name" "Service" config
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
