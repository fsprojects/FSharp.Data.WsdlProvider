module FSharp.Data.Xsd

open System.Xml
open System.Xml.Linq
open System.Xml.Schema



type XName with
    member this.QualifiedName =
        XmlQualifiedName(this.LocalName, this.NamespaceName)

type XmlQualifiedName with
    member this.XName =
        XName.Get(this.Name, this.Namespace)


type MinOccurs = MinOccurs of int
type MaxOccurs = MaxOccurs of int  | Unbounded

type Occurs = 
    { Min: MinOccurs 
      Max: MaxOccurs}

type XsElement =
    { Name: XName
      Type: XsTypeRef
      DefaultValue: string option
      Occurs: Occurs
      Nillable: bool
      SubstitutionGroup: XName option}
and XsParticle =
    | XsElement of XsElement
    | XsAny of Occurs
    | XsChoice of Choice
and Choice =
    { Items: XsParticle list
      Occurs: Occurs }
and XsTypeRef =
    | TypeRef of XName
    | InlineType of XsType
and Ordrer =
    | NoContent
    | Sequence of XsParticle list * Occurs
    | All of XsParticle list * Occurs
    | Choice of XsParticle list * Occurs
and XsComplexType =
    { BaseType: XName option
      Elements: Ordrer
      Attributes: XsAttribute list
      AnyAttribute: bool
      Mixed: bool }
and XsSimpleType =
    { BaseType: XName
      Enumeration: XsEnum list }
and XsEnum =
    { Value: string
      Name: string }
and XsType =
    | XsComplexType of XsComplexType
    | XsSimpleType of XsSimpleType
and XsAttribute =
    { Name: XName
      Type: XsAttributeType
      DefaultValue: string option
      Use: XmlSchemaUse }
and XsAttributeType =
    | XsSimple of XName
    | XsList of XName
and XsTypeDef =
    { Name: XName
      Type: XsType }

module XsType =
    let empty = 
        { BaseType = None
          Elements = NoContent
          Attributes = []
          AnyAttribute = false
          Mixed = false }

module Occurs =
    let once = 
        { Min = MinOccurs 1
          Max = MaxOccurs 1}
    let optional = 
        { Min = MinOccurs 0
          Max = MaxOccurs 1}

let parseOccurs (p: XmlSchemaParticle) =
    { Min = MinOccurs (int p.MinOccurs)
      Max = 
          if p.MaxOccurs = System.Decimal.MaxValue then
              Unbounded
          else
              MaxOccurs(int p.MaxOccurs) }

let rec parseElement (e: XmlSchemaElement) =
    let t =
        if isNull e.SchemaTypeName || e.SchemaTypeName.IsEmpty then
            match e.ElementSchemaType with
            | :? XmlSchemaSimpleType as t when t.QualifiedName <> XmlQualifiedName.Empty ->
                TypeRef t.QualifiedName.XName
            | t ->
                parseType t
                |> InlineType 
        else
            TypeRef e.SchemaTypeName.XName
            
    let d =
        e.DefaultValue
        |> Option.ofObj
    
    let fix =
        e.FixedValue
        |> Option.ofObj

    { XsElement.Name = e.QualifiedName.XName
      Type = t
      DefaultValue =  d |> Option.orElse fix
      Occurs = parseOccurs e
      Nillable = e.IsNillable
      SubstitutionGroup = 
        if e.SubstitutionGroup = XmlQualifiedName.Empty then
            None
        else
            Some e.SubstitutionGroup.XName }
and parseChoice (choice: XmlSchemaChoice) =
    let particles = 
        choice.Items
        |> Seq.cast<XmlSchemaObject>
        |> Seq.choose(fun i -> 
            match i with 
            | :? XmlSchemaParticle as p -> Some p
            | _ -> None)
                
    { Items = [ for item in particles do
                    yield parseParticle item ]
      Occurs = 
        {Min = MinOccurs (int choice.MinOccurs)
         Max = MaxOccurs (int choice.MaxOccurs) } }
    
and parseParticle (p: XmlSchemaParticle) =
    match p with
    | :? XmlSchemaElement as e -> XsElement (parseElement e)
    | :? XmlSchemaAny as any -> XsAny (parseOccurs any)
    | :? XmlSchemaChoice as choice -> XsChoice (parseChoice choice)
    | _ -> failwithf "Unknown particle"
and parseType (t: XmlSchemaType) =
    match t with
    | :? XmlSchemaComplexType as t ->
        XsComplexType 
          { BaseType = 
                if isNull t.BaseXmlSchemaType then
                    None
                else
                    let n = t.BaseXmlSchemaType.QualifiedName
                    if n = XmlQualifiedName("anyType", "http://www.w3.org/2001/XMLSchema") then
                        None
                    else
                        Some n.XName

            Elements = 
                let occurs = parseOccurs t.ContentTypeParticle
                match t.ContentTypeParticle with
                | :? XmlSchemaSequence as s ->
                    let ps =
                        s.Items
                        |> Seq.cast<XmlSchemaParticle>
                        |> Seq.map parseParticle
                        |> Seq.toList
                    Sequence(ps, occurs)
                | :? XmlSchemaAll as s ->
                    let ps =
                        s.Items
                        |> Seq.cast<XmlSchemaElement>
                        |> Seq.map parseParticle
                        |> Seq.toList
                    All(ps, occurs)
                | :? XmlSchemaChoice as s ->
                    let ps = 
                        s.Items
                        |> Seq.cast<XmlSchemaElement>
                        |> Seq.map parseParticle
                        |> Seq.toList
                    Choice(ps, occurs)
                | _ -> NoContent

                
            Attributes = 
                t.AttributeUses.Values
                |> Seq.cast<XmlSchemaAttribute>
                |> Seq.map parseAttribute
                |> Seq.toList
            AnyAttribute = not (isNull t.AnyAttribute)
            Mixed = t.IsMixed
        }
    | :? XmlSchemaSimpleType as t ->
        let enums =
            match t.Content with
            | :? XmlSchemaSimpleTypeRestriction as r ->
                r.Facets
                |> Seq.cast<XmlSchemaFacet>
                |> Seq.choose ( fun facet ->
                    match facet with
                    | :? XmlSchemaEnumerationFacet as e ->
                        let doc = 
                            if isNull e.Annotation  then
                                e.Value
                            else
                                e.Annotation.Items
                                |> Seq.cast<XmlSchemaObject>
                                |> Seq.tryPick (fun a ->
                                    match a with
                                    | :? XmlSchemaDocumentation as d ->
                                        Some (
                                            
                                            d.Markup
                                            |> Array.map (fun m -> m.InnerText)
                                            |> String.concat(" "))
                                    | _ -> None
                                )
                                |> Option.defaultValue e.Value
                        
                        Some { Value = e.Value; Name = doc }
                    | _ -> None)
                |> Seq.toList

            | _ -> []
        XsSimpleType

            { BaseType = t.BaseXmlSchemaType.QualifiedName.XName
              Enumeration = enums
              }




    | _ -> failwith "Unknown type"


and parseAttribute (a: XmlSchemaAttribute) =
    { Name = a.QualifiedName.XName
      Type = 
        if a.SchemaTypeName = XmlQualifiedName.Empty then
            match a.AttributeSchemaType.Content with
            | :? XmlSchemaSimpleTypeList as l ->
                XsList l.ItemTypeName.XName
            | :? XmlSchemaSimpleTypeRestriction as r ->
                XsSimple r.BaseTypeName.XName

            | _ -> failwith "Unsupported type"

        else
            XsSimple a.SchemaTypeName.XName
             
      DefaultValue = (Option.ofObj a.DefaultValue) |> Option.orElse (Option.ofObj a.FixedValue)
      Use = a.Use 
    }
and parseTypeDef (t: XmlSchemaType) =
    { Name = t.QualifiedName.XName
      Type = parseType t}
   
open System.Collections.Generic
[<CustomEquality; NoComparison>]
type XsSet =
    { Types: IDictionary<XName, XsTypeDef>
      Elements: IDictionary<XName, XsElement> }
    
    interface System.Collections.IStructuralEquatable with
        member this.Equals(other, comparer) =
            match other with
            | :? XsSet as s ->
                if s.Types.Count <> this.Types.Count || s.Elements.Count <> this.Elements.Count then
                    false
                else
                    this.Types
                    |> Seq.forall (fun (KeyValue(k,v)) -> 
                        match s.Types.TryGetValue(k) with
                        | false, _ -> false
                        | true, v' -> comparer.Equals(v,v'))
                    && this.Elements
                       |> Seq.forall (fun (KeyValue(k,v)) -> 
                           match s.Elements.TryGetValue(k) with
                           | false, _ -> false
                           | true, v' -> comparer.Equals(v,v'))

            | _ -> false

        member this.GetHashCode(comparer) =
            let t = 
                this.Types
                |> Seq.fold (fun h (KeyValue(k,v)) -> (h * 357 + comparer.GetHashCode(k)) * 13 + comparer.GetHashCode(v) ) 0
            let e = 
                this.Elements
                |> Seq.fold (fun h (KeyValue(k,v)) -> (h * 357 + comparer.GetHashCode(k)) * 13 + comparer.GetHashCode(v) ) 0

            357 * e + t

    
module Schema =
    let element (xname: XName) (set: XmlSchemaSet) = 
        set.GlobalElements[xname.QualifiedName]
        :?> XmlSchemaElement
        |> parseElement
        
    let typeDef (xname: XName) (set: XmlSchemaSet) = 
        set.GlobalTypes[xname.QualifiedName]
        :?> XmlSchemaType
        |> parseTypeDef
       

    let set (set: XmlSchemaSet) =
        { Elements = 
            set.GlobalElements.Values
            |> Seq.cast<XmlSchemaElement>
            |> Seq.map parseElement
            |> Seq.map (fun e -> e.Name, e)
            |> dict 
          Types = 
            set.GlobalTypes.Values
            |> Seq.cast<XmlSchemaType>
            |> Seq.map parseTypeDef
            |> Seq.map (fun t -> t.Name , t)
            |> dict
        }


    let builtInSimpleType (n: XName) =
        let t = XmlSchemaSimpleType.GetBuiltInSimpleType(n.QualifiedName)
        if isNull t then
            None
        else
            Some t.Datatype.ValueType

    let isBuiltInSimpleType (n: XName) =
        XmlSchemaSimpleType.GetBuiltInSimpleType(n.QualifiedName)
        |> isNull
        |> not

            
let ns = XNamespace.Get XmlSchema.Namespace

