namespace FSharp.Data.WsdlGenerator

open System
open System.Text
open FSharp.Data
open FSharp.Data.Wsdl
open Myriad.Core
open Myriad.Core.Ast
open FSharp.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.Xml
open FSharp.Compiler.Text
open System.ServiceModel
open System.Collections.Generic
open System.ServiceModel.Channels
open FSharp.Compiler.SyntaxTrivia
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


        
type DefaultBinding = 
    static member SelectBinding(uri: string) =
        if (Uri uri).Scheme = Uri.UriSchemeHttps then
            BasicHttpsBinding() :> Binding
        else
            BasicHttpBinding() :> Binding
module Generation = 
    open System.Xml.Linq
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
        | :? TRef as v -> $"typeof<{v.Name}>"
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
        | "member" -> $"``{id}``"
        | _ -> id

    let mkdAttribute<'t>(args : (Type*obj) list) (namedArgs: (string * obj) list) (builder: Builder)  = 
        let t = typeof<'t>
        let attrArgs = 
            seq { for t,arg in args do 
                    cst arg
                  for (name, arg) in namedArgs do
                    $"{name} = {cst arg}" }
        builder.StartAppend($"[<{t.Namespace}.{cleanAttribute t.Name}(")
                .AppendJoin(", ", attrArgs)
                .AppendLine(")>]")
                

    let mkXmlElementAttribute (order: int) =  
        mkdAttribute<XmlElementAttribute> [] [ "Order", box order ]

    let mkXmlElementNameAttribute (name: XName, t: TRef) =
        mkdAttribute<XmlElementAttribute> [typeof<string>, box name.LocalName]
            [ "Type", box t
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
        

    //type ClientContext =
    //    { Channel: Expr -> Expr
    //      ClientInterface: ProvidedTypeDefinition
    //      SoapInterface: ProvidedTypeDefinition
    //      Client: ProvidedTypeDefinition }

    let taskType (tref: TRef) =
        match tref.Name with
        | "unit" -> "Task"
        | n -> $"Task<{n}>"

    let defineOperationMethod (op: Operation, input: (string * TRef) list , output: TRef) (builder: Builder) =
        let name = op.PortOperation.Name
        builder.StartAppend($"member this.{name}(")
               .AppendJoin(", ", [ for name, t in input -> $"{cleanId name}: {t.Name}"])
               .AppendLine($") : {output.Name} =")
               .Indent()
                   .StartAppend($"base.Channel.{name}(")
                   .AppendJoin(", ", [ for name,_ in input -> cleanId name ])
                   .AppendLine(")")

               .Unindent()


    let defineAsyncOperationMethod (op: Operation,input: (string*TRef) list, output: TRef) (builder: Builder) =
        let name = op.PortOperation.Name + "Async"
        builder.StartAppend($"member this.{name}(")
               .AppendJoin(", ", [ for name, t in input -> $"{cleanId name}: {t.Name}"])
               .AppendLine($") : {taskType output} =")
               .Indent()
                   .StartAppend($"base.Channel.{name}(")
                   .AppendJoin(", ", [ for name,_ in input -> cleanId name ])
                   .AppendLine(")")

               .Unindent()

    let defineSoapItfOperationMethod (op: Operation, input: (string * TRef) list , output: TRef) (builder: Builder) =
        let name = op.PortOperation.Name
        match input with
        | [] ->
            builder.Indent()
                   .StartAppend($"abstract {name}: unit -> {output.Name}")
                   .Unindent()

        | _ ->

            builder.Indent()
                   .StartAppend($"abstract {name}: ")
                   .AppendJoin("* ", [ for name, t in input -> $"{t.Name}"])
                   .AppendLine($" -> {output.Name}")
                   .Unindent()


    let defineSoapItfAsyncOperationMethod (op: Operation,input: (string*TRef) list, output: TRef) (builder: Builder) =
        let name = op.PortOperation.Name + "Async"
        match input with
        | [] ->
            builder.Indent()
                   .StartAppend($"abstract {name}: unit -> {taskType output}")
                   .Unindent()


        | _ ->
            builder.Indent()
                   .StartAppend($"abstract {name}: ")
                   .AppendJoin(", ", [ for name, t in input -> $"{t.Name}"])
                   .AppendLine($" -> {taskType output}")
                   .Unindent()

    //    // method on soap interface alwasy use tasks (include soap attributes)
    //    let taskOutput = 
    //        if output = typeof<Unit> then
    //            typeof<Task>
    //        else
    //            ProvidedTypeBuilder.MakeGenericType(typedefof<Task<_>>, [ output ])

    //    let soapItfMeth = ProvidedMethod(name, input , taskOutput)
    //    soapItfMeth.AddCustomAttribute (mkOperationContractAttribute op.SoapAction "*")
    //    soapItfMeth.AddCustomAttribute(mkXmlSerializerFormatAttribute())

    //    clientCtx.SoapInterface.AddMember(soapItfMeth)

    //    // method on front facing interface using task
    //    let itfTaskMeth = ProvidedMethod(name, input , taskOutput)
    //    clientCtx.ClientInterface.AddMember(itfTaskMeth)

    //    // implementation of the task method
    //    let code = 
    //        fun (args: Expr list) ->
    //            Expr.Call(clientCtx.Channel args.[0], soapItfMeth, args.[1..])

    //    let clientMeth = ProvidedMethod(name, input, taskOutput, code)

    //    clientCtx.Client.AddMember(clientMeth)
    //    let itfImpl = ProvidedMethod(clientCtx.ClientInterface.FullName + "." + name, input , taskOutput, invokeCode = ( fun args -> Expr.Call(args.[0], clientMeth, args.[1..])))
    //    itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
    //    clientCtx.Client.DefineMethodOverride(itfImpl, itfTaskMeth)
    //    clientCtx.Client.AddMember(itfImpl)

    //    // method on front facing interface using Async
    //    let asyncName = "Async" + op.PortOperation.Name
    //    let asyncOutput = ProvidedTypeBuilder.MakeGenericType(typedefof<Async<_>>, [ output ])
    //    let itfAsyncMeth = ProvidedMethod(asyncName, input , asyncOutput)
    //    clientCtx.ClientInterface.AddMember(itfAsyncMeth)

    //    let code = 

    //        let awaitTask = 
    //            if output = typeof<Unit> then
    //                typeof<Async>.GetMethod("AwaitTask", [| typeof<Task>|])
    //            else
    //                let awaitTaskGen = 
    //                    typeof<Async>.GetMethods()
    //                    |> Seq.find(fun m -> 
    //                        m.Name = "AwaitTask" 
    //                        && m.IsGenericMethod 
    //                        && (let ps = m.GetParameters() 
    //                            ps.Length = 1
    //                            && ps.[0].ParameterType.IsGenericType
    //                            && ps.[0].ParameterType.GetGenericTypeDefinition() = typedefof<Task<_>>) )
    //                ProvidedTypeBuilder.MakeGenericMethod(awaitTaskGen, [output])

    //        fun (args: Expr list) ->
    //            Expr.Call(awaitTask, [ Expr.Call(clientCtx.Channel args.[0], soapItfMeth, args.[1..] ) ])

    //    let clientMeth = ProvidedMethod(asyncName, input, asyncOutput, code)

    //    clientCtx.Client.AddMember(clientMeth)
    //    let itfImpl = ProvidedMethod(clientCtx.ClientInterface.FullName + "." + asyncName, input , asyncOutput, invokeCode = ( fun args -> Expr.Call(args.[0], clientMeth, args.[1..])))
    //    itfImpl.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual ||| MethodAttributes.Final)
    //    clientCtx.Client.DefineMethodOverride(itfImpl, itfAsyncMeth)
    //    clientCtx.Client.AddMember(itfImpl)


    //// builds a log2 deep Sequential tree from expression list
    //// this is required for constructors that take many arguments.
    //// The output sequence is used by a non-tail call function that will
    //// raise stackoverflow if the nesting is too deep. Using a binary split
    //// enables lower depth.
    //let rec makeSequential (exprs: Expr list) =
    //    match exprs with
    //    | [] -> <@@ () @@>
    //    | [ e ] -> e
    //    | [ ex; ey] -> Expr.Sequential(ex,ey)
    //    | _ ->
    //        let left, right = List.splitAt(exprs.Length / 2) exprs
    //        Expr.Sequential( makeSequential left, makeSequential right)


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
                    cleanId (String.camlCase name)
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

    let buildWsdlTypes nsp name wsdl =

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

                //if not (List.isEmpty all) then

                //    let fields =
                //        [ for e in all do
                //            match e with
                //            | CTElement(name, _, t)
                //            | CTAttribute(name, _, t)
                //            | CTArray(name, _, t, _) ->
                //                ProvidedField( String.camlCase name, t )
                //            | CTChoice _ -> 
                //                ProvidedField( "item", typeof<obj> )
                //                ]

                //    let props =
                //        (all,fields)
                //        ||> List.mapi2 (fun i e field ->
                //           let name,t =
                //                match e with
                //                | CTElement(name,_,t)
                //                | CTAttribute(name,_,t)
                //                | CTArray(name,_,t,_) -> name,t
                //                | CTChoice _ -> "Item", typeof<obj>
                                    
                           
                //           let prop = ProvidedProperty(name, t, getterCode = (fun args -> Expr.FieldGet( args.[0], field) ), setterCode = (fun args -> Expr.FieldSet(args.[0], field, args.[1] ))) 
                //           match e with
                //           | CTElement(_, xsname,_)  -> 
                //                if contract then
                //                    prop.AddCustomAttribute(mkMessageBodyMember(xsname.NamespaceName , i))
                //                else
                //                    prop.AddCustomAttribute(mkXmlElementAttribute i)
                //           | CTAttribute(_, xsname,_) -> prop.AddCustomAttribute(mkXmlAttributeAttribute xsname)
                //           | CTArray(_, xsname,_, itemName) ->
                //                if contract then
                //                    prop.AddCustomAttribute(mkMessageBodyMember(xsname.NamespaceName , i))
                //                else
                //                    prop.AddCustomAttribute(mkXmlArrayAttribute i)
                //                prop.AddCustomAttribute(mkXmlArrayItemAttribute(itemName, false))
                //           | CTChoice choices ->
                //                for c in choices do
                //                    match c with
                //                    | CTElement(_,xsname,t) ->
                //                        prop.AddCustomAttribute(mkXmlElementNameAttribute(xsname,t))
                //                    | CTAttribute(_,xsname,t) ->
                //                        prop.AddCustomAttribute(mkXmlAttributeNameAttribute(xsname,t))
                //                    | CTArray _
                //                    | CTChoice _ -> ()    
                //           prop
                //        )

                //    match props |> Seq.countBy (fun p -> p.Name) |> Seq.filter(fun (n,c) -> c > 1) |> Seq.toList with
                //    | [] -> ()
                //    | l -> failwithf "Property defined twice: %s (all: %d, props: %d)" (l |> List.map (fun (n,c) -> $"{n}: {c}") |> String.concat "," ) all.Length props.Length


                //    let ctor = 
                //        let ps =
                //            [ for e in all do
                //                match e with
                //                | CTElement(name,_, t)
                //                | CTAttribute(name,_, t)
                //                | CTArray(name,_, t,_) ->
                //                    ProvidedParameter(String.camlCase name, t)
                //                | CTChoice _ ->
                //                    ProvidedParameter("item", typeof<obj>) ]

                //        ProvidedConstructor(ps, fun args -> 
                //            let this = args.[0]
                //            let sets = 
                //                fields |> List.mapi (fun i field ->
                //                    Expr.FieldSet(this, field, args.[i+1] ))

                //            makeSequential sets
                            
                //        )

                //    pt.AddMembers(fields)
                //    pt.AddMembers(props)
                //    pt.AddMember(ctor)

                //let pt = ProvidedTypeDefinition(asm, p.Namespace , typeName, Some typeof<obj>, isErased = false)
                let td = 
                    match xmlname with
                    | XmlType n ->
                        if contract then
                            Contract { TypeName = typeName; XmlName = n; Members = all  } 
                            //pt.AddCustomAttribute(mkMessageContractAttribute(n, true))
                        else
                            ComplexType { TypeName = typeName; XmlName = n; Members = all  } 
                            //pt.AddCustomAttribute(mkXmlTypeAttribute(n.NamespaceName, false))
                    | Anonymous n ->
                        AnonymousType { TypeName = typeName ; XmlName = n; Members = all }
                        //pt.AddCustomAttribute(mkXmlTypeAttribute(n.NamespaceName, true))
                    | NoName -> 
                        
                        NoNameType { TypeName = typeName; XmlName = null; Members = all}
                types.Add(td)


                tr
                //(pt :> Type)

        //and flattenComplexType contract name (xmlname: CTXmlName) (t: XsComplexType) =
        //    printfn $"Flatten CT {xmlname}"
        //    match xmlname with
        //    | XmlType n 
        //    | Anonymous n ->
        //        match typeRefs.TryGetValue(n) with
        //        | true, t -> t |> Some
        //        | false, _ -> None
        //    | NoName  ->
        //        match types.TryGetValue(XName.Get n) with
        //        | true, t -> t |> Some
        //        | false, _ -> None
        //    |> function
        //       | Some t -> ()
        //       | None ->
            
        //            let typeName = fixTypeName name 
        //            match xmlname with
        //            | XmlType n ->
        //                let tcdef = 
        //                    { TypeName = typeName
        //                      XmlName = n
        //                      ComplexType = t}
        //                let tdef = if contract then Contract tcdef else ComplexType tcdef
        //                types.Add(n, tdef)
        //            | Anonymous n -> 
        //                let tdef =
        //                    AnonymousType
        //                        { TypeName = typeName
        //                          XmlName = n
        //                          ComplexType = t}
        //                types.Add(n, tdef)

        //            | NoName name ->
        //                let xmlName = XName.Get name
        //                let tdef =
        //                    NoNameType
        //                        { TypeName = name
        //                          XmlName = xmlName
        //                          ComplexType = t
        //                          }
        //                types.Add(xmlName, tdef)
                        

        //            match t.Elements with
        //            | Sequence elts ->
        //                for p in elts do
        //                    let rec flattenCT p =

        //                        match p with
        //                        | XsElement { Type = TypeRef t }  
        //                        | XsElement { Type = InlineType (XsSimpleType { BaseType = t }) } ->
                                        
        //                            let innerType = 
        //                                if Schema.isBuiltInSimpleType t then
        //                                    None
        //                                else
        //                                    Some wsdl.Schemas.Types.[t]

        //                            match innerType with
        //                            | Some { Type = XsComplexType { Attributes = []; Elements = Sequence [ XsElement { Name = itemName; Occurs = { Max = max }; Type = TypeRef titem } ]}} when max > MaxOccurs 1 ->
        //                                // This is actually an array
        //                                flattenTypeRef titem
        //                            | Some { Name = arrayName ; Type = XsComplexType { Attributes = []; Elements = Sequence [ XsElement { Name = itemName; Occurs = { Max = max }; Type = InlineType (XsComplexType ct) } ]}} when max > MaxOccurs 1 ->
                                    
        //                                flattenComplexType false (String.PascalCase arrayName.LocalName + String.PascalCase itemName.LocalName) (Anonymous arrayName) ct

        //                            | _ -> flattenTypeRef t

                                             
                                              
        //                        | XsElement ( { Type = InlineType (XsComplexType ct)} as e) ->
        //                                let localName = name + String.PascalCase e.Name.LocalName
        //                                flattenComplexType false localName (NoName localName) ct
                                        

        //                        | XsAny _ -> ()
        //                        | XsChoice choices ->
        //                            for item in choices.Items do
        //                                flattenCT item

        //                    flattenCT p
                                    

                                   
        //                | _ -> ()

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

        and buildMember i (m: CTChild) (builder: Builder) =
            let mkAttribute m =
                match m with
                | CTElement(_, xsname,_)  -> 
                        mkXmlElementAttribute i
                | CTContract(_, xsname,_)  -> 
                        mkMessageBodyMember(xsname.NamespaceName , i)
                | CTAttribute(_, xsname,_) -> mkXmlAttributeAttribute xsname
                | CTArray(_, xsname,_, itemName) ->
                        mkXmlArrayAttribute i
                        >> mkXmlArrayItemAttribute(itemName, false)
                | CTArrayContract(_, xsname,_, itemName) ->
                    mkMessageBodyMember(xsname.NamespaceName , i)
                    >> mkXmlArrayItemAttribute(itemName, false)

                | CTChoice choices ->
                        
                    Builder.fold (fun c -> 
                        match c with
                        | CTElement(_,xsname,t)
                        | CTContract(_,xsname,t) ->
                            mkXmlElementNameAttribute(xsname,t)
                        | CTAttribute(_,xsname,t) ->
                            mkXmlAttributeNameAttribute(xsname,t)
                        | CTArray _
                        | CTArrayContract _
                        | CTChoice _ -> id
                    )  choices

            builder.Apply(mkAttribute, m).StartLine($"member val {m.PropName} : { m.TypeName } = {cleanId m.FieldName} with get, set")

        and buildComplexType (t: ComplexTypeDef) (builder: Builder) =
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
                        .AppendJoin(", ", [ for  m in t.Members -> m.FieldName ])
                        .AppendLine(") =")
                        .Indent()
                        .Foldi(buildMember, t.Members)
                        .Unindent()
                        .AppendLine()


        and buildEnum (t: EnumTypeDef) (builder: Builder) =

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
                


        and buildType (typeDef: TypeDef) (builder: Builder) : Builder =
            match typeDef with
            | Contract t ->
                builder
                |> mkMessageContractAttribute(t.XmlName , true)
                |> buildComplexType t
            | ComplexType t ->
                builder
                |> mkXmlTypeAttribute(t.XmlName.NamespaceName, false)
                |> buildComplexType t
            | AnonymousType t -> 
                builder
                |> mkXmlTypeAttribute(t.XmlName.NamespaceName, true)
                |> buildComplexType t
            | NoNameType t -> 
                builder
                |> buildComplexType t
            | EnumType t ->
                builder
                |> mkXmlTypeAttribute(t.XmlName.NamespaceName,false)
                |> buildEnum t

            
        and simpleTypeRef (t:Type) =
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

        let flattenPort port =
            for op in port.Binding.Operations do
               flattenOperation op 

        let flattenService service =
            for port in service.Ports do
                flattenPort port

        //let buildMessage contract (name: XName) (typeRef: XsTypeRef) =
        //    match typeRef with
        //    | TypeRef name 
        //    | InlineType (XsSimpleType { BaseType = name}) ->
        //        match Schema.builtInSimpleType name with
        //        | Some t -> t
        //        | None  ->
        //            match types.TryGetValue(name) with
        //            | true, t -> ()
        //            | false,_ ->                    
        //                buildType contract wsdl.Schemas.Types.[name]
        //    | InlineType (XsComplexType t) ->
        //        match types.TryGetValue(name) with
        //        | true, t -> t :> Type
        //        | false,_ ->                    
        //            buildComplexType contract name.LocalName (XmlType name) t
                
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

        //let buildElement contract (e: XsElement) =
        //    match e with
        //    | { Type = t   ; Occurs = { Max = Unbounded }} ->
        //        (buildMessage contract  e.Name t).MakeArrayType()
        //    | { Type = InlineType (XsComplexType { Elements = Sequence [ XsElement { Type = TypeRef t} ] })} when not contract  ->
        //        buildMessage contract e.Name (TypeRef t)
        //    | { Type = InlineType (XsSimpleType { BaseType = t}) } when not contract ->
        //        buildMessage contract e.Name (TypeRef t)
        //    | { Type = InlineType (XsComplexType { BaseType = None; Elements = NoContent; Mixed = false; Attributes = []; AnyAttribute = false}) } ->
        //        typeof<unit>
        //    | { Type = ct } ->
        //        buildMessage contract e.Name ct
                
        //    | _ -> failwithf "Canot build toplevel element %O" e.Name
            
        let buildOperation op (builder: Builder) =

            // synchronous method
            builder
            |> defineOperationMethod op
            // task method
            |> defineAsyncOperationMethod op

        let buildSoapItfOperation op (builder: Builder) =
            builder
            |> defineSoapItfOperationMethod op
            |> defineSoapItfAsyncOperationMethod op
           
        let mkInterface name (builder: Builder) =
            
            builder
            |> mkdAttribute<InterfaceAttribute> [] []
            |> fun builder ->
                builder.StartLine($"type {name} =")




        let buildPort serviceName (port: Port) (builder: Builder) = 
            let soapItfName = "I" + port.Name.LocalName


            let declareType (builder: Builder) =
                let args = 
                    [ $"binding: {typeof<Binding>.FullName}"
                      $"address: {typeof<EndpointAddress>.FullName}"]
                builder.StartAppend($"type {serviceName}(")
                    .AppendJoin(", ", args)
                    .AppendLine(") =")
                    .Indent()
                    .StartLine($"inherit System.ServiceModel.ClientBase<{soapItfName}>(binding, address)")


            let ops =
                [ for op in port.Binding.Operations ->
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
                    op, input, output
                ]

            builder
            |> mkServiceContractAttribute (port.Name.NamespaceName, nsp + "." + serviceName)
            |> mkInterface soapItfName 
            |> Builder.fold buildSoapItfOperation ops
            |> Builder.newLine
            |> mkInterface port.Name.LocalName
            |> Builder.fold buildSoapItfOperation ops
            |> Builder.newLine
            |> declareType
            |> defineCtors serviceName port.Location
            |> defineLocation port.Location
            |> Builder.fold buildOperation ops
            

            //let channelProp = clientBase.GetProperty("Channel", BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetProperty)

            //let channel this = 
            //    Expr.PropertyGet(this, channelProp)

            //let clientCtx =
            //    { Channel = channel
            //      ClientInterface = itf
            //      SoapInterface = soapItf
            //      Client = client }

            //for op in port.Binding.Operations do
            //    buildOperation op clientCtx



        let selectBinding (builder: Builder) =
            builder.StartLine("type DefaultBinding = ")
                    .StartLine(1, "static member SelectBinding(uri: string) =")
                    .StartLine(2, "if (Uri uri).Scheme = Uri.UriSchemeHttps then")
                    .StartLine(3, $"{typenameof<BasicHttpsBinding>}() :> {typenameof<Binding>}")
                    .StartLine(2, "else")
                    .StartLine(3, $"{typenameof<BasicHttpBinding>}() :> {typenameof<Binding>}")
                    .AppendLine()
            
            
        
 
        let buildService (service : Service) (builder: Builder)  = 
            builder
            |> selectBinding
            |> Builder.fold (buildPort service.Name) service.Ports


        for service in wsdl.Services do
            flattenService service


        { Indentation = 0; Writer = StringBuilder()}
            .StartLine($"namespace rec {nsp}")
            .AppendLine()
            .StartLine("open System")
            .StartLine("open System.Threading.Tasks")
            .AppendLine()
            .Fold((fun (td: TypeDef) -> buildType td) , Seq.cast types)
            .Fold(buildService, wsdl.Services)
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

            let source = Generation.buildWsdlTypes ns name  wsdl
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
