module FSharp.Data.ClientModel

open System
open System.Collections.Generic
open System.Xml.Linq
open Xsd
open Wsdl

let private typeNames =
    readOnlyDict [
        typeof<obj>, "obj"
        typeof<bool>, "bool"
        typeof<int>, "int"
        typeof<uint>, "uint"
        typeof<int64>, "int64"
        typeof<uint64>, "uint64"
        typeof<int16>, "int16"
        typeof<uint16>, "uint16"
        typeof<byte>, "byte"
        typeof<sbyte>, "sbyte"
        typeof<double>, "double"
        typeof<float>, "float"
        typeof<string>, "string"
    ]

type TRef =
    | TSimple of Type
    | TRef of string
    | TEnum of string
    | TRArray of TRef
    | TRNullable of TRef
    with
    member this.IsStruct =
        match this with
        | TSimple t -> t.IsValueType
        | TRef _ -> false
        | TEnum _ -> true 
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
        | TSimple t ->
            t.FullName 
        | TRef t -> t
        | TEnum t -> t
        | TRArray t -> t.Name + "[]"
        | TRNullable t -> $"Nullable<{t.Name}>"

    member this.FsName =
        match this with
        | TSimple t ->
            match typeNames.TryGetValue(t) with
            | true, name -> name
            | false, _ -> t.FullName

        | TRef t -> t
        | TEnum t -> t
        | TRArray t -> t.FsName + "[]"
        | TRNullable t -> $"Nullable<{t.FsName}>"

    member this.IsUnit =
        match this with
        | TSimple t -> t = typeof<unit>
        | _ -> false


    static member Unit = TSimple typeof<unit>

type CTChild =
    | CTElement of string * XName * TRef * int
    | CTContract of string * XName * TRef * int
    | CTAttribute of string * XName * TRef 
    | CTArray of string * XName * TRef * string * int
    | CTArrayContract of string * XName * TRef * string * int
    | CTChoice of CTChild list * int
    | CTArrayChoice of CTChild list * int
    | CTAny 
    with
        member this.FieldName =
            match this with
            | CTElement(name, _,_,_)
            | CTContract(name, _,_,_)
            | CTAttribute(name, _,_)
            | CTArray(name, _,_,_,_)
            | CTArrayContract(name, _,_,_,_) ->
                 String.camlCase name + "Field"
            | CTChoice _ -> "item"
            | CTArrayChoice _ -> "items"
            | CTAny -> "item"

        member this.PropName =
            match this with
            | CTElement(name, _,_,_)
            | CTContract(name, _,_,_)
            | CTAttribute(name, _,_)
            | CTArray(name, _,_,_,_)
            | CTArrayContract(name, _,_,_,_) ->
                name
            | CTChoice _ -> "Item"
            | CTArrayChoice _ -> "Items"
            | CTAny _ -> "Item"
        member this.TypeName =
            match this with
            | CTElement(_, _,t,_)
            | CTContract(_, _,t,_)
            | CTAttribute(_, _,t)
            | CTArray(_, _,t,_,_)
            | CTArrayContract(_, _,t,_,_) ->
                t.Name 
            | CTChoice _ -> "System.Object"
            | CTArrayChoice _ -> "System.Object[]"
            | CTAny _ -> "System.Object"

        member this.FsTypeName =
            match this with
            | CTElement(_, _,t,_)
            | CTContract(_, _,t,_)
            | CTAttribute(_, _,t)
            | CTArray(_, _,t,_,_)
            | CTArrayContract(_, _,t,_,_) ->
                t.FsName 
            | CTChoice _ -> "obj"
            | CTArrayChoice _ -> "obj[]"
            | CTAny _ -> "obj"

        member this.TypeRef =
            match this with
            | CTElement(_, _,t,_)
            | CTContract(_, _,t,_)
            | CTAttribute(_, _,t)
            | CTArray(_, _,t,_,_)
            | CTArrayContract(_, _,t,_,_) ->
                t
            | CTChoice _ -> TSimple typeof<obj>
            | CTArrayChoice _ -> TRArray(TSimple typeof<obj>)
            | CTAny _ -> TSimple typeof<obj>

        member this.MakeArrayType() =
            match this with
            | CTElement(name, xn, t, i) -> CTElement(name, xn, t.MakeArrayType(), i)
            | CTContract(name, xn, t,i) -> CTContract(name, xn, t.MakeArrayType(), i)
            | CTAttribute(name, xn, t) -> CTAttribute(name, xn, t.MakeArrayType())
            | CTArray(name, xn, t, ixn, i) -> CTArray(name, xn, t.MakeArrayType(), ixn, i)
            | CTArrayContract(name, xn, t, ixn, i) -> CTArrayContract(name, xn, t.MakeArrayType(), ixn, i)
            | CTChoice(choices,i) -> CTArrayChoice(choices, i)
            | CTArrayChoice _ -> this
            | CTAny _ -> this

type CTXmlName =
    | XmlType of XName
    | Anonymous of XName
    | NoName

type ComplexTypeDef =
    { TypeName: string
      XmlName: XName
      Members: CTChild list }

type EnumValue =
    { Name: string
      Value: string
      Index: int
      }
type EnumTypeDef =
    { TypeName: string
      XmlName: XName
      BaseType: TRef
      Values: EnumValue list }

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
      Action: string
      Input: (string * TRef) list
      Output: TRef }

type PortDef =
    { Name: string
      Namespace: string
      Location: string
      Operations: OperationDef list }
type ServiceDef =
    { Name: string
      Ports: PortDef list }

type WsdlDef =
    { Types: TypeDef list
      Services: ServiceDef list }


   


let flattenWsdl wsdl =
    let typeRefs = Dictionary<XName, TRef>()
    let typeNames = Dictionary<string,int>()
    let types = ResizeArray<TypeDef>()


    //// avoid name clashes by adding a number suffix to existing type names
    let fixTypeName name =
        match typeNames.TryGetValue(name) with
        | false, _ ->
            typeNames[name] <- 0
            name
        | true, n ->
            let newN = n + 1
            typeNames[name] <- newN
            name + string newN


    let rec flattenComplexType contract name (xmlname: CTXmlName) (t: XsComplexType) = 
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
            let tr = TRef typeName
            match xmlname with
            | XmlType n ->
                typeRefs.Add(n, tr)
            | Anonymous n ->
                typeRefs.Add(n, tr)
            | NoName -> ()

            let rec getCT contract i p =
                match p with
                | XsElement ({ Type = TypeRef t } as e) 
                | XsElement ({ Type = InlineType (XsSimpleType { BaseType = t }) } as e) ->
                    

                    let innerType = 
                        if Schema.isBuiltInSimpleType t then
                            None
                        else
                            Some wsdl.Schemas.Types[t]

                    match innerType with
                    | Some { Type = XsComplexType { Attributes = []; Elements = Sequence([ XsElement { Name = itemName; Occurs = { Max = max }; Type = TypeRef titem } ], lseqOccurs)}} when max > MaxOccurs 1 ->
                        // This is actually an array
                        let ref = typeRef titem
                        let propType = ref.MakeArrayType()
                        let md = (e.Name.LocalName, e.Name, propType, itemName.LocalName, i)
                        if contract then
                            CTArrayContract md
                        else
                            CTArray md
                    | Some { Name = arrayName ; Type = XsComplexType { Attributes = []; Elements = Sequence([ XsElement { Name = itemName; Occurs = { Max = max }; Type = InlineType (XsComplexType ct) } ], lseqOccurs)}} when max > MaxOccurs 1 ->
                    
                        let pt = flattenComplexType false (String.PascalCase arrayName.LocalName + String.PascalCase itemName.LocalName) (Anonymous arrayName) ct
                        let propType = pt.MakeArrayType()
                        CTArray (e.Name.LocalName, e.Name, propType, itemName.LocalName, i)

                    | _ ->

                        let propType =
                            (typeRef t) 
                            |> applyElementCardinality e 
                        let md = (e.Name.LocalName, e.Name, propType, i)
                        if contract then
                            CTContract md
                        else
                            CTElement md

                         
                          
                | XsElement ( { Type = InlineType (XsComplexType ct)} as e) ->
                    let propType = 
                        match ct with
                        | { Elements = Sequence([ XsAny _ ], _ ) } ->
                            TSimple typeof<obj>
                        | _ ->
                            flattenComplexType false (name + String.PascalCase e.Name.LocalName) NoName ct
                            |> applyElementCardinality e
                        
                    let md = (e.Name.LocalName, e.Name, propType, i)
                    if contract then
                        CTContract md
                    else
                        CTElement md
                | XsAny _ -> CTAny
                | XsChoice choices ->
                    CTChoice (choices.Items |> List.map (getCT false i), i)
                

            let elements = 
                match t.Elements with
                | Sequence((_ :: _ :: _) as choices, { Max = MaxOccurs.Unbounded}) ->
                    [CTArrayChoice(choices |> List.map (getCT contract 0), 0)]
                | Sequence([item], { Max = MaxOccurs.Unbounded}) ->
                    let ct = getCT contract 0 item 

                    [ ct.MakeArrayType() ]

                | Sequence(elts, seqOccurs) ->
                    [ for i,p in elts |> Seq.indexed do
                        getCT contract i p
                           
                    ]
                | _ -> []



            let elementNames = set [ for e in elements -> e.PropName 
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
            | Some ty -> TSimple ty
            | None -> failwith "Unsupported simple type base type"


    and flattenEnum (name: XName) (t: XsSimpleType) =
        match typeRefs.TryGetValue(name) with
        | true, t -> t
        | false, _ ->
            let typeName = fixTypeName name.LocalName

            let td = 
                EnumType { 
                    TypeName = typeName
                    XmlName = name
                    BaseType = typeRef t.BaseType
                    Values = [ for i,v in Seq.indexed t.Enumeration -> { Name = v.Name; Value = v.Value; Index = i } ]}
            types.Add(td)
            let tr = TEnum typeName
            typeRefs.Add(name, tr )
            tr


    and applyCardinality cardinality (ref: TRef) =
        if cardinality.Max > MaxOccurs 1 then
            ref.MakeArrayType()
        elif cardinality.Min = MinOccurs 0 then
            ref.MakeNullableType()
        else
            ref
    and applyElementCardinality e (ref: TRef) =
        if e.Occurs.Max > MaxOccurs 1 then
            ref.MakeArrayType()
        elif e.Nillable then
            ref.MakeNullableType()
        else
            ref

    and typeRef (name: XName) : TRef =
        match Schema.builtInSimpleType name with
        | Some t -> TSimple t
        | None  ->
            match typeRefs.TryGetValue(name) with
            | true, t -> t
            | false,_ -> flattenType false wsdl.Schemas.Types[name]



    and attributeTypeRef (t: XsAttributeType) =
        match t with
        | XsSimple t -> typeRef t
        | XsList t -> (typeRef t).MakeArrayType()



    let flattenMessage contract (name: XName) (typeRef: XsTypeRef) =
        if not(typeRefs.ContainsKey name) then
            match typeRef with
            | TypeRef btname 
            | InlineType (XsSimpleType { BaseType = btname}) ->
                let ct =
                    match typeRefs.TryGetValue(btname) with
                    | true, t -> t
                    | _ ->
                        match Schema.builtInSimpleType btname with
                        | Some t ->
                            let ct = TSimple t
                            typeRefs.Add(btname, ct)
                            ct
                        | None  ->
                            flattenType contract wsdl.Schemas.Types[btname]

                if not (typeRefs.ContainsKey name) then
                    // this check is necessary because the call to flattenType above can add it
                    typeRefs.Add(name, ct)

            | InlineType (XsComplexType t) ->
                match typeRefs.TryGetValue(name) with
                | true, t -> ()
                | false,_ ->                    
                    flattenComplexType contract name.LocalName (XmlType name) t |> ignore

    let flattenElement contract (e: XsElement) =
        match e with
        | { Type = t   ; Occurs = { Max = Unbounded }} ->
            flattenMessage contract  e.Name t
        | { Type = InlineType (XsComplexType { Elements = Sequence([ XsElement { Type = TypeRef t} ], _) })} when not contract  ->
            flattenMessage contract e.Name (TypeRef t)
        | { Type = InlineType (XsSimpleType { BaseType = t}) } when not contract ->
            flattenMessage contract e.Name (TypeRef t)
        | { Type = InlineType (XsComplexType { BaseType = None; Elements = NoContent; Mixed = false; Attributes = []; AnyAttribute = false}) } ->
            let name = e.Name
            let typeName = fixTypeName name.LocalName
            let def = { TypeName = typeName; XmlName = name; Members = []}
            let ct = 
                if contract then
                    Contract def
                else
                    ComplexType def
            types.Add(ct)
            typeRefs.Add(name, TRef typeName)
            
        | { Type = ct } ->
            flattenMessage contract e.Name ct

    let flattenParameters contract (name: XName) (t: XsTypeRef) =
        match t with
        | InlineType (XsComplexType { Elements = NoContent; BaseType = None}) ->
            // no content, this is a void method
            ()
        | InlineType (XsComplexType { Elements = Sequence([ XsElement ({ Type = tn} as elt) ],_)}) ->
            // No need for all this wrapping. Just use the element/type inside
            flattenMessage contract name tn 
        | tn ->
            flattenMessage contract name tn 


    let flattenOperation op =
        try
            let contract = op.PortOperation.RequireContract
            match op.PortOperation.Input with
            | None  -> ()
            | Some {Element = Element { Name = n; Type = t }} -> 
                flattenParameters contract n t
            | Some { Name = n; Element = SimpleType t } ->
                flattenParameters contract (XName.Get n) (TypeRef t)

            match op.PortOperation.Output with
            | Some {Element = Element e } ->
                flattenElement contract e 
            | Some {Element = SimpleType t } ->
                typeRef t |> ignore
            | None ->
                ()
        with
        | ex -> raise (Exception($"Failed operation {op.PortOperation.Name}", ex))

    let flattenPort (port: Port) =
        for op in port.Binding.Operations do
           flattenOperation op 

    let flattenService (service: Service) =
            for port in service.Ports do
                flattenPort port 

    for service in wsdl.Services do
        flattenService service


    types, typeRefs
let wsdlTypesNs = XNamespace.Get "http://microsoft.com/wsdl/types/"
let createModel wsdl =

    let types, typeRefs = flattenWsdl wsdl


    let findTypeRef (name: XName) : TRef =
        match Schema.builtInSimpleType name with
        | Some t -> TSimple t
        | None  ->
            if name.Namespace = wsdlTypesNs then
                match name.LocalName with
                | "guid" -> TSimple typeof<Guid>
                | _ -> failwithf $"unknown wsdl type {name}"
            else
                match typeRefs.TryGetValue(name) with
                | true, t -> t
                | false,_ -> failwith $"TypeRef {name} not found"

    let buildParameters (name: XName) (t: XsTypeRef) =
        match t with
        | InlineType (XsComplexType { Elements = NoContent; BaseType = None}) ->
            // no content, this is a void method
            []
        | InlineType (XsComplexType { Elements = Sequence([ XsElement ({ Type = TypeRef tn} as elt) ],_)}) ->
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
          Action = op.SoapAction
          Input = input
          Output = output }

    let buildPort (port: Port)  = 
        { Name = port.Name.LocalName
          Namespace = port.Name.Namespace.NamespaceName
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



