module FSharp.Data.Wsdl

open System.Xml.Linq

type PrimitiveXsdType = 
    | XsdString
    | XsdBool
    | XsdByte
    | XsdShort
    | XsdInt
    | XsdLong
    | XsdDecimal
    | XsdFloat
    | XsdDouble
    | XsdDate
    | XsdTime
    | XsdDateTime
    | XsdHexBinary
    | XsdBase64Binary


type Occurs =
    | Occurs of int
    | Unbounded

type WsdlType =
    | Element of Element
    | Primitive of PrimitiveXsdType
    | ComplexType of ComplexType
    | TypeRef of string
    | EmptyType


/// Represents an element (field/arg) in a complex type
and Element = 
    { MinOccurs: Occurs
      MaxOccurs: Occurs
      Name: string
      Type: WsdlType }
/// Represents a complex type
and ComplexType =
    { Name: string option
      Elements: Element list }

/// Represents a Wsdl operation (Soap action)
type Operation =
    { Name: string
      SoapAction: string
      Documentation: string option
      /// the operation input argument (as a complex type)
      Input: Element 
      /// the operation return type 
      Output: Element }

// A soap binding (
type SoapBinding =
    { Name: string
      Type: string
      Operations: Operation list}

// A wsdl port (a binding at a specific location)
type Port =
    { Name : string
      Location: string
      Binding: SoapBinding }

// A Wsdl service, a named list of wsdl ports
type Service =
    { Name: string
      Ports: Port list}

type Wsdl =
    { TargetNamespace: string
      Types: WsdlType list 
      Services: Service list }


module String =
    let split (char: char) (str: string) = 
        str.Split(char)

module Xml =
    let xn = XName.Get
    let attr name (elt: XElement) = elt.Attribute(xn name).Value
    let tryAttr name (elt: XElement) = 
        elt.Attribute(xn name)
        |> Option.ofObj
        |> Option.map (fun a -> a.Value)
    let tryElt name (elt: XElement) =
        elt.Element(name)
        |> Option.ofObj

let (|XAtt|_|) (a: XAttribute) =
    if isNull a then
        None
    else
        Some a.Value

module XmlSchema =
    let ns = XNamespace.Get "http://www.w3.org/2001/XMLSchema"

    let schema = ns + "schema" 
    let element = ns + "element" 
    let complexType = ns + "complexType" 
    let sequence = ns + "sequence"

    module Attr =
        let name = XName.Get "name"
        let ``type`` = XName.Get "type"
        let minOccurs = XName.Get "minOccurs"
        let maxOccurs = XName.Get "maxOccurs"

module Wsdl =
    let ns = XNamespace.Get "http://schemas.xmlsoap.org/wsdl/"

    let service = ns + "service"
    let port = ns + "port"
    let message= ns + "message"
    let part = ns + "part"
    let types = ns + "types"
    let operation =  ns + "operation"
    let documentation =  ns + "documentation"
    let input = ns + "input"
    let output = ns + "output"
    let binding = ns + "binding"
    let portType = ns + "portType"
    
    module Attr =
        let name = XName.Get "name"
        let location = XName.Get "location"
        let binding = XName.Get "binding"
        let targetNamespace = XName.Get "targetNamespace"


module Soap =
    let ns = XNamespace.Get "http://schemas.xmlsoap.org/wsdl/soap/"

    let operation = ns + "operation"
    let address = ns + "address"

    module Attr = 
        let soapAction =  XName.Get "soapAction"


let tryParsePrimitive s (input: string) =
    match input.Split(':') with
    | [| prefix ; "boolean" |] when prefix = s -> Some XsdBool
    | [| prefix ; "string" |] when prefix = s -> Some XsdString
    | [| prefix ; "int" |] when prefix = s -> Some XsdInt
    | [| prefix ; "decimal" |] when prefix = s -> Some XsdDecimal
    | [| prefix ; "byte" |] when prefix = s -> Some XsdByte
    | [| prefix ; "short" |] when prefix = s -> Some XsdShort
    | [| prefix ; "long" |] when prefix = s -> Some XsdLong
    | [| prefix ; "dateTime" |] when prefix = s -> Some XsdDateTime
    | [| prefix ; "date" |] when prefix = s -> Some XsdDate
    | [| prefix ; "time" |] when prefix = s -> Some XsdTime
    | [| prefix ; "base64Binary" |] when prefix = s -> Some XsdBase64Binary
    | [| prefix ; "hexBinary" |] when prefix = s -> Some XsdHexBinary
    | [| prefix ; "float" |] when prefix = s -> Some XsdFloat
    | [| prefix ; "double" |] when prefix = s -> Some XsdDouble
    | _ -> None

let parseRef tns s =
    match String.split ':' s with
    | [| prefix ; name |] when prefix = tns -> Some (TypeRef name)
    | _ -> None

let attrOrDefault (name: XName) f def (e: XElement) =
    let att = e.Attribute(name)
    if isNull att then
       def
    else
        att.Value |> f

let parsOccurs (s: string) =
    if s = "unbounded" then
        Unbounded
    else
        Occurs (int s)

let parseType s tns t =
    tryParsePrimitive s t
    |> Option.map Primitive
    |> Option.orElseWith (fun _ -> parseRef tns t)

let rec parseElement tns (e: XElement) =
    let name = e.Attribute(XmlSchema.Attr.name).Value
    let minOccurs = e |> attrOrDefault XmlSchema.Attr.minOccurs parsOccurs (Occurs 0)
    let maxOccurs = e |> attrOrDefault XmlSchema.Attr.maxOccurs parsOccurs Unbounded

    let s = e.GetPrefixOfNamespace(XmlSchema.ns)
    let tnsp = e.GetPrefixOfNamespace(tns)
    let typ =
        match e.Attribute(XmlSchema.Attr.``type``) with
        | XAtt t -> 
            let simpleType = parseType s tnsp t

            match simpleType with
            | Some t -> t
            | None -> failwithf "Unknown type %s" t

        | _ ->
            e.Element(XmlSchema.complexType)
            |> Option.ofObj
            |> Option.map (parseComplexType tns)
            |> Option.defaultValue EmptyType
    { Name = name
      MinOccurs = minOccurs
      MaxOccurs = maxOccurs
      Type = typ }

and parseComplexType tns (e:XElement) =
    let name = e |> attrOrDefault XmlSchema.Attr.name Some None
    let sequence = e.Element(XmlSchema.sequence)

    let elts =
        if isNull sequence then
            []
        else
            [ for elt in sequence.Elements()
               ->
                parseElement tns elt ]

    ComplexType
        { Name = name
          Elements = elts}

let parseMessage tns (wsdl: XDocument) fullname =
    let tnsp = wsdl.Root.GetPrefixOfNamespace(tns)
    match String.split ':' fullname with
    | [| ns; name |] when ns = tnsp ->
        let message= 
            wsdl.Root.Elements(Wsdl.message)
            |> Seq.tryFind (fun e -> Xml.attr "name" e = name)

        match message with
        | None -> failwithf "Could not find message %s in wsdl" name
        | Some msg -> 
            let fullElementName =
                msg.Element(Wsdl.part)
                |> Xml.attr "element"
            let s = msg.GetPrefixOfNamespace(XmlSchema.ns)
            let tnsp = msg.GetPrefixOfNamespace(tns)

            match String.split ':' fullElementName with
            | [| ns; elementName|] when ns = tnsp ->
                let element = 
                    wsdl.Root.Element(Wsdl.types)
                        .Elements(XmlSchema.schema)
                        .Elements(XmlSchema.element)
                    |> Seq.tryFind (fun e -> Xml.attr "name" e = elementName)

                match element with
                | Some e -> parseElement tns e
                | None -> failwithf "Could not find element %s" elementName


            | _ -> failwithf "Could not find element %s" fullElementName
            
            
    | _ -> failwithf "Cannot find message %s in wsdl" fullname



let parseOperation tns (wsdl: XDocument) (portType: XElement)  (wsdlOp: XElement) =
    let name = wsdlOp.Attribute(Wsdl.Attr.name).Value
    let op = wsdlOp.Element(Soap.operation)
    let action = op.Attribute(Soap.Attr.soapAction).Value

    let portOp = 
        portType.Elements(Wsdl.operation)
        |> Seq.find(fun e -> e.Attribute(Wsdl.Attr.name).Value = name)

    let doc = 
        portOp.Element(Wsdl.documentation)
        |> Option.ofObj
        |> Option.map(fun x -> x.Value)
    let input =
        portOp.Element(Wsdl.input)
        |> Xml.attr "message"
        |> parseMessage tns wsdl
    let output =
        portOp.Element(Wsdl.output)
        |> Xml.attr "message"
        |> parseMessage tns wsdl
    { Name = name
      SoapAction = action
      Documentation = doc
      Input =  input 
      Output = output  }


let parseBinding tns (wsdl: XDocument) name =
    let name = 
        match String.split ':' name with
        | [| _ ; n |] -> n
        | _ -> name

    let e = 
        wsdl.Root.Elements(Wsdl.binding)
        |> Seq.find(fun b -> Xml.attr "name" b = name)

    let t = 
        let t =  e |> Xml.attr "type" 
        match String.split ':' t with
        | [| _ ; n |] -> n
        | _ -> t

    let portType = 
        wsdl.Root.Elements(Wsdl.portType)
        |> Seq.find (fun e -> Xml.attr "name" e = t )

    { Name = name
      Type = t
      Operations =
        e.Elements(Wsdl.operation)
        |> Seq.map (parseOperation tns wsdl portType)
        |> Seq.toList }

    
let parsePort tns wsdl (e: XElement) =
    match e.Element(Soap.address) with

    | null -> None
    | address -> 
        let name = e.Attribute(Wsdl.Attr.name).Value
        let location = address.Attribute(Wsdl.Attr.location).Value
        let bindingName = e.Attribute(Wsdl.Attr.binding).Value
        let binding = parseBinding tns wsdl bindingName

        Some 
            { Name = name
              Location = location
              Binding = binding }

let parseServices tns (wsdl: XDocument) = 
    [ for e in wsdl.Root.Elements(Wsdl.service) ->
        { Name = e.Attribute(Wsdl.Attr.name).Value
          Ports = e.Elements(Wsdl.port) |> Seq.choose (parsePort tns wsdl) |> Seq.toList }
    ]

let parseTypes tns (schema: XElement) =
    [ for e in schema.Elements() do
        if e.Name = XmlSchema.element then
            parseElement tns e |> Element
        elif e.Name = XmlSchema.complexType then
            parseComplexType tns e
    ]


let parse (wsdl: XDocument) =
    let tns = wsdl.Root.Attribute(Wsdl.Attr.targetNamespace).Value |> XNamespace.Get
    let types = wsdl.Root.Element(Wsdl.types)
    let schema = types.Element(XmlSchema.schema)

    let types = parseTypes tns schema
    let services = parseServices tns wsdl
    { TargetNamespace = string tns
      Types =  types
      Services = services }

