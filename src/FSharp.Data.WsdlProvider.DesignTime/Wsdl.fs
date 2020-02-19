module FSharp.Data.Wsdl

open System.Xml
open System.Xml.Linq
open System.Xml.Schema

type MessageElement =
    | SimpleType of XmlSchemaSimpleType
    | Element of XmlSchemaElement 


type Part =
    { Name: string
      Element: MessageElement }

type Message =
    { Name: XName
      Part: Part option }

type PortOperation =
    { Name: string
      Documentation: string option
      /// the operation input argument (as a complex type)
      Input: Part option
      /// the operation return type 
      Output: Part option }
type PortType =
    { Name: XName
      Operations: PortOperation list}

/// Represents a Wsdl operation (Soap action)
type Operation =
    { PortOperation: PortOperation
      SoapAction: string }

// A soap binding
type SoapBinding =
    { Name: XName
      Operations: Operation list }

// A wsdl port (a binding at a specific location)
type Port =
    { Name : XName
      Location: string
      Binding: SoapBinding }

// A Wsdl service, a named list of wsdl ports
type Service =
    { Name: string
      Ports: Port list}

type Wsdl =
    { Schemas: XmlSchemaSet
      Services: Service list }


module String =
    let split (char: char) (str: string) = 
        str.Split(char)

type XName with
    member this.QualifiedName =
        XmlQualifiedName(this.LocalName, this.NamespaceName)

type XmlQualifiedName with
    member this.XName =
        XName.Get(this.Name, this.Namespace)

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

let (|XAtt|_|) name (e: XElement) =
    let a = e.Attribute(XName.Get name)
    if isNull a then
        None
    else
        Some a.Value

let resolveXName (e: XElement) name =
    match String.split ':' name with
    | [| pfx; localName |] ->
        let ns = e.GetNamespaceOfPrefix(pfx)
        Some (ns + localName)
    | _ -> None
    
let (|XName|_|) e name = resolveXName e name

let (|XsdElement|_|) (e: XmlSchemaObject) =
    match e with
    | :? XmlSchemaElement as e -> Some e
    | _ -> None

let (|XsdSequence|_|) (e: XmlSchemaObject) =
    match e with
    | :? XmlSchemaSequence as s -> Some (s.Items |> Seq.cast<XmlSchemaElement> |> Seq.toList)
    | _ -> None

let (|XsdComplexType|_|) (e: XmlSchemaObject) =
    match e with
    | :? XmlSchemaComplexType as t -> Some t
    | _ -> None

let (|XsdSimpleType|_|) (e: XmlSchemaObject) =
    match e with
    | :? XmlSchemaSimpleType as t -> Some t
    | _ -> None

let (|Particle|) (t: XmlSchemaComplexType) =
    t.Particle

let (|SchemaType|) (e: XmlSchemaElement) =
    e.ElementSchemaType

let (|XsdArray|_|) (e: XmlSchemaObject) =
    match e with
    | XsdComplexType (Particle (XsdSequence [XsdElement e])) 
        when e.MinOccurs = 0m && e.MaxOccurs = System.Decimal.MaxValue
        -> Some e.ElementSchemaType
    | _ -> None

let (|XsdEmptyType|_|) (e: XmlSchemaObject) =
    match e with
    | XsdComplexType t when isNull t.Particle ->
        Some()
    | XsdComplexType (Particle (XsdSequence [])) ->
        Some()
    | _ -> None
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

let parseMessage (tns: XNamespace) (msg: XElement ) (schemas: XmlSchemaSet) =
    let msgName = Xml.attr "name" msg
    let partElt =
        msg.Element(Wsdl.part)
        |> Option.ofObj

    let part = 
        match partElt with
        | None -> None
        | Some p ->
            let partName = Xml.attr "name" p
            let element =
                match p with
                | XAtt "element" (XName p name) ->
                     schemas.GlobalElements.[name.QualifiedName]
                     :?> XmlSchemaElement
                     |> Element
                | XAtt "type" (XName p name) ->
                    XmlSchemaSimpleType.GetBuiltInSimpleType(name.QualifiedName)
                    |> SimpleType
                | _ -> failwithf "Could not find element for %s" msgName
            Some { Name = partName
                   Element = element }
        
    { Name = tns + msgName
      Part =  part }
            

let parseMessages tns (wsdl: XDocument) (schemas: XmlSchemaSet) =
    [ for e in  wsdl.Root.Elements(Wsdl.message) ->
        parseMessage tns e schemas ]


let parseOperation (messages: Message list) (portOp: XElement) (*wsdlOp: XElement*)  =
    let name = portOp.Attribute(Wsdl.Attr.name).Value

    let doc = 
        portOp.Element(Wsdl.documentation)
        |> Option.ofObj
        |> Option.map(fun x -> x.Value)
    let input =
        let inputElement = portOp.Element(Wsdl.input)
        match Xml.attr "message" inputElement with
        | XName inputElement name -> 
            messages
            |> List.find (fun m -> m.Name = name)
        | n -> failwithf "Could not parse input message name %s" n

        


    let output =
        let outputElement = portOp.Element(Wsdl.output)

        match Xml.attr "message" outputElement with
        | XName outputElement name ->
            messages
            |> List.find (fun m -> m.Name = name)
        | n -> failwithf "Could not parse output message name %s" n


    { Name = name
      Documentation = doc
      Input = input.Part
      Output = output.Part  }

let parsePortTypes (tns: XNamespace) (wsdl: XDocument) messages =
    [ for p in wsdl.Root.Elements(Wsdl.portType) ->
        let name = Xml.attr "name" p
        { PortType.Name = tns + name
          Operations = 
            [for op in p.Elements(Wsdl.operation) ->
                parseOperation messages op ] 
        }
    ]
   

let parseBinding (tns: XNamespace) (binding: XElement) (portTypes: PortType list) =
    let name = Xml.attr "name" binding
    let t = 
        match Xml.attr "type"  binding with
        | XName binding t -> t
        | t -> XName.Get t

    let portType = 
        portTypes
        |> List.find (fun e -> e.Name = t )

    let ops = 
        [ for wsdlop in binding.Elements(Wsdl.operation) do
            let opName = Xml.attr "name" wsdlop
            let soapOp = wsdlop.Element(Soap.operation)
            if not (isNull  soapOp) then
                let action = Xml.attr "soapAction" soapOp
                let pop = portType.Operations |> List.find (fun o -> o.Name = opName)
                { SoapAction = action
                  PortOperation = pop } ]


    { Name = tns + name
      Operations = ops }


let parseBindings tns (wsdl: XDocument) portTypes =
    [ for b in wsdl.Root.Elements(Wsdl.binding) ->
        parseBinding tns b portTypes ]

    
    
let parsePort (bindings: SoapBinding list) (tns: XNamespace) (e: XElement)  =
    match e.Element(Soap.address) with

    | null -> None
    | address -> 
        let name = e.Attribute(Wsdl.Attr.name).Value
        let location = address.Attribute(Wsdl.Attr.location).Value
        let bindingName =  
            match e.Attribute(Wsdl.Attr.binding).Value with
            | XName e n -> n
            | t -> XName.Get t
        let binding =
            bindings
            |> List.find (fun b -> b.Name = bindingName)

        Some 
            { Name = tns + name
              Location = location
              Binding = binding }

let parseServices bindings tns (wsdl: XDocument)  = 
    [ for e in wsdl.Root.Elements(Wsdl.service) ->
        { Name = e.Attribute(Wsdl.Attr.name).Value
          Ports = 
            e.Elements(Wsdl.port)
            |> Seq.choose (parsePort bindings tns)
            |> Seq.toList }
    ]


let rec parseWsdlImports (wsdl: XDocument) docs =
    wsdl.Root.Elements(Wsdl.ns + "import")
    |> Seq.fold 
        (fun docs import ->
            let ns = Xml.attr "namespace" import
            if Map.containsKey ns docs then
                docs
            else
                let location = Xml.attr "location" import 
                let doc = XDocument.Load(location)

                docs
                |> Map.add ns doc
                |> parseWsdlImports doc ) 
        docs


let parse (wsdl: XDocument) =
    let tns = wsdl.Root.Attribute(Wsdl.Attr.targetNamespace).Value |> XNamespace.Get

    let imports = 
        Map.empty
        |> Map.add tns.NamespaceName wsdl
        |> parseWsdlImports wsdl
        |> Map.toList


    let schemas = XmlSchemaSet(XmlResolver = XmlUrlResolver() )
    for ns,wsdl in imports  do
        let types = wsdl.Root.Element(Wsdl.types)
        for schema in types.Elements(XmlSchema.schema) do
            schemas.Add(null, schema.CreateReader()) |> ignore

    schemas.Compile()

    let messages =
        [ for ns,wsdl in imports do
            yield! parseMessages (XNamespace.Get ns) wsdl schemas ]

    let portTypes =
        [ for ns,wsdl in imports do
            yield! parsePortTypes (XNamespace.Get ns) wsdl messages ] 

    let bindings =
        [ for ns, wsdl in imports do
            yield! parseBindings (XNamespace.Get ns) wsdl portTypes]


    let services = 
        [ for ns, wsdl in imports do
            yield! parseServices bindings (XNamespace.Get ns) wsdl  ]

    { Schemas = schemas
      Services = services }

