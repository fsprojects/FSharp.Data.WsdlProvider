module FSharp.Data.Wsdl

open System
open System.Xml
open System.Xml.Linq
open FSharp.Data.Xsd

type MessageElement =
    | SimpleType of XName
    | Element of XsElement 

type Part =
    { Name: string
      Element: MessageElement }

type Message =
    { Name: XName
      Part: Part option }
type PortOperation =
    { Name: string
      Documentation: string option
      InputName: string option
      OutputName: string option
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

/// Binding style
type BindingStyle =
    | Document
    | RPC

// A soap binding
[<RequireQualifiedAccess>]
type SoapVersion =
    | Soap11
    | Soap12

type SoapBinding =
    { Name: XName
      Style: BindingStyle
      Version: SoapVersion
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
    { Schemas: XsSet
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

module XmlSchema =
    let ns = XNamespace.Get "http://www.w3.org/2001/XMLSchema"

    let schema = ns + "schema" 
    let element = ns + "element" 
    let complexType = ns + "complexType" 
    let sequence = ns + "sequence"

    let _name = XName.Get "name"
    let _type = XName.Get "type"
    let _minOccurs = XName.Get "minOccurs"
    let _maxOccurs = XName.Get "maxOccurs"

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

    let _name = XName.Get "name"
    let _location = XName.Get "location"
    let _binding = XName.Get "binding"
    let _targetNamespace = XName.Get "targetNamespace"

module Soap =
    let ns = XNamespace.Get "http://schemas.xmlsoap.org/wsdl/soap/"
    let binding = ns + "binding"
    let operation = ns + "operation"
    let body = ns + "body"
    let fault = ns + "fault"
    let header = ns + "header"
    let headerfault = ns + "headerfault"
    let address = ns + "address"

    let _transport =  XName.Get "transport"
    let _style =  XName.Get "style"
    let _soapAction =  XName.Get "soapAction"
    let _encodingStyle =  XName.Get "encodingStyle"
    let _use =  XName.Get "use"
    let _namespace =  XName.Get "namespace"
    let _parts =  XName.Get "parts"
    let _name =  XName.Get "name"
    let _message =  XName.Get "message"
    let _part =  XName.Get "part"
    let _location =  XName.Get "location"
module Soap12 =
    let ns = XNamespace.Get "http://schemas.xmlsoap.org/wsdl/soap12/"
    let binding = ns + "binding"
    let operation = ns + "operation"
    let body = ns + "body"
    let fault = ns + "fault"
    let header = ns + "header"
    let headerfault = ns + "headerfault"
    let address = ns + "address"

    let _transport =  XName.Get "transport"
    let _style =  XName.Get "style"
    let _soapAction =  XName.Get "soapAction"
    let _encodingStyle =  XName.Get "encodingStyle"
    let _use =  XName.Get "use"
    let _namespace =  XName.Get "namespace"
    let _parts =  XName.Get "parts"
    let _name =  XName.Get "name"
    let _message =  XName.Get "message"
    let _part =  XName.Get "part"
    let _location =  XName.Get "location"

let parseMessage (tns: XNamespace) (msg: XElement ) (schemas: XsSet) =
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
                     schemas.Elements.[name]
                     |> Element
                | XAtt "type" (XName p name) ->
                    name
                    |> SimpleType
                | _ -> failwithf "Could not find element for %s" msgName
            Some { Name = partName
                   Element = element }
        
    { Name = tns + msgName
      Part =  part }
            
let parseMessages tns (wsdl: XElement) (schemas: XsSet) =
    [ for e in  wsdl.Elements(Wsdl.message) ->
        parseMessage tns e schemas ]

let parseOperation (messages: Message list) (portOp: XElement) (*wsdlOp: XElement*)  =
    let name = portOp.Attribute(Wsdl._name).Value
    let messageName (elementName: XName) =
        portOp.Element(elementName)
        |> Option.ofObj
        |> Option.bind (Xml.tryAttr "name")
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

    { 
        Name = name
        Documentation = doc
        InputName = messageName Wsdl.input
        OutputName = messageName Wsdl.output
        Input = input.Part
        Output = output.Part
    }

let parsePortTypes (tns: XNamespace) (wsdl: XElement) messages =
    [ for p in wsdl.Elements(Wsdl.portType) ->
        let name = Xml.attr "name" p
        { PortType.Name = tns + name
          Operations = 
            [for op in p.Elements(Wsdl.operation) ->
                parseOperation messages op ] 
        }
    ]
   
let parseBinding
    (tns: XNamespace)
    (binding: XElement)
    (portTypes: PortType list) =

    // Identify the SOAP version declared by the WSDL binding.
    let version, soapBindingElement =
        match Xml.tryElt Soap12.binding binding with
        | Some element ->
            SoapVersion.Soap12, element

        | None ->
            match Xml.tryElt Soap.binding binding with
            | Some element ->
                SoapVersion.Soap11, element

            | None ->
                failwith "Unsupported SOAP binding."

    // Resolve the QName referencing the abstract portType.
    let name = Xml.attr "name" binding

    let portTypeName =
        match Xml.attr "type" binding with
        | XName binding name -> name
        | name -> XName.Get name

    let portType =
        portTypes
        |> List.find (fun candidate ->
            candidate.Name = portTypeName
        )

    // SOAP operation elements must belong to the same
    // SOAP extension vocabulary as the binding.
    let soapOperationElementName =
        match version with
        | SoapVersion.Soap11 -> Soap.operation
        | SoapVersion.Soap12 -> Soap12.operation

    // Parse the concrete operation bindings.
    let operations =
        [
            for wsdlOperation in binding.Elements(Wsdl.operation) do

                let operationName =
                    Xml.attr "name" wsdlOperation

                let soapOperation =
                    match Xml.tryElt soapOperationElementName wsdlOperation with
                    | Some element -> element
                    | None ->
                        failwithf
                            "Missing SOAP operation binding: %s"
                            operationName

                let messageName elementName =
                    Xml.tryElt elementName wsdlOperation
                    |> Option.bind (Xml.tryAttr "name")

                let inputName = messageName Wsdl.input
                let outputName = messageName Wsdl.output

                let matchesWhenSpecified expected actual =
                    match expected with
                    | None -> true
                    | Some name -> actual = Some name

                // Resolve the abstract operation, including overload identity.
                let portOperation =
                    portType.Operations
                    |> List.filter (fun candidate ->
                        candidate.Name = operationName
                        && matchesWhenSpecified inputName candidate.InputName
                        && matchesWhenSpecified outputName candidate.OutputName
                    )
                    |> function
                        | [operation] ->
                            operation

                        | [] ->
                            failwithf
                                "No matching portType operation: %s"
                                operationName

                        | _ ->
                            failwithf
                                "Ambiguous portType operation: %s"
                                operationName

                yield {
                    SoapAction = Xml.attr "soapAction" soapOperation
                    PortOperation = portOperation
                }
        ]

    // Read SOAP-specific binding style from the SOAP binding element.
    let style =
        match Xml.tryAttr "style" soapBindingElement with
        | Some "rpc" -> RPC
        | _ -> Document

    {
        Name = tns + name
        Style = style
        Version = version
        Operations = operations
    }
let parseBindings tns (wsdl: XElement) portTypes =
    [ for b in wsdl.Elements(Wsdl.binding) ->
        parseBinding tns b portTypes ]

let parsePort (bindings: SoapBinding list) (tns: XNamespace) (e: XElement)  =

    let address =
        match e.Element(Soap.address) with
        | null -> e.Element(Soap12.address)
        | element -> element

    match address with
    | null -> None
    | address ->
        let name = e.Attribute(Wsdl._name).Value
        let location = address.Attribute(Wsdl._location).Value
        let bindingName =  
            match e.Attribute(Wsdl._binding).Value with
            | XName e n -> n

            | t -> XName.Get t
        let binding =
            bindings
            |> List.find (fun b -> b.Name = bindingName)

        Some 
            { Name = tns + name
              Location = location
              Binding = binding }

let parseServices bindings tns (wsdl: XElement)  = 
    [ for e in wsdl.Elements(Wsdl.service) ->
        { Name = e.Attribute(Wsdl._name).Value
          Ports = 
            e.Elements(Wsdl.port)
            |> Seq.choose (parsePort bindings tns)
            |> Seq.toList }
    ]

let rec parseWsdlImports (wsdl: XElement) baseUri (resolver: XmlResolver) docs =
    wsdl.Elements(Wsdl.ns + "import")
    |> Seq.fold 
        (fun docs import ->
            let ns = Xml.attr "namespace" import
            let location = Uri(Xml.attr "location" import, UriKind.RelativeOrAbsolute)
            let uri =
                if location.IsAbsoluteUri then
                    location
                else
                    Uri(baseUri, location)
            if Map.containsKey (ns, string uri) docs then
                docs
            else
                let doc = 
                    resolver.GetEntity(uri, "wsdl", null)
                    |> unbox<IO.Stream>
                    |> XDocument.Load

                docs
                |> Map.add (ns, string uri) doc.Root
                |> parseWsdlImports doc.Root uri resolver ) 
        docs

open System.Xml.Schema

let nsName (ns: string) suffix =
    let uri = Uri ns
    if uri.AbsoluteUri.StartsWith("http://schemas.datacontract.org/2004/07/",StringComparison.Ordinal) then
        uri.LocalPath.Substring(9).Replace("/",".").TrimEnd('.') + suffix
    else
        uri.Authority + uri.LocalPath.Replace("/",".").TrimEnd('.') + suffix

let writeLocalSchema (w: XmlTextWriter) (imports: ((string * string) * XElement) list) (schemas: XmlSchemaSet) =
    w.WriteStartDocument()
    w.WriteStartElement("ServiceMetadataFiles")

    let importsXsdNs =
        set [ for _,wsdl in imports do
              for tns in wsdl.Descendants(Xsd.ns + "schema").Attributes(XName.Get "targetNamespace") ->
              tns.Value ]

    for (ns,_),wsdl in imports do
        w.WriteStartElement("ServiceMetadataFile")
        w.WriteAttributeString("name", nsName ns ".wsdl" )
        wsdl.WriteTo(w)
        w.WriteEndElement()
    for s  in schemas.Schemas() |> Seq.cast<XmlSchema> do
        if not (Set.contains s.TargetNamespace importsXsdNs) then
            w.WriteStartElement("ServiceMetadataFile")
            w.WriteAttributeString("name", nsName s.TargetNamespace ".xsd" )
            s.Write(w)
            w.WriteEndElement()
    w.WriteEndElement()

let saveLocalSchema file indent imports schemas =
    use writer = IO.File.CreateText(file)
    let w = new XmlTextWriter(writer, Formatting = if indent then Formatting.Indented else Formatting.None)
    writeLocalSchema w imports schemas

let dontSave _ _ = ()

let parseWithLoader (wsdl: XElement) documentUri loader saveSchema =
    let tns = wsdl.Attribute(Wsdl._targetNamespace).Value |> XNamespace.Get

    let imports = 
        Map.empty
        |> Map.add (tns.NamespaceName, string documentUri) wsdl
        |> parseWsdlImports wsdl documentUri loader
        |> Map.toList

    let schemas = XmlSchemaSet(XmlResolver = loader )
    for _,wsdl in imports  do
        let types = wsdl.Elements(Wsdl.types)
        for schema in types.Elements(XmlSchema.schema) do
            schemas.Add(null, schema.CreateReader()) |> ignore

    schemas.Compile()
    let schemaSet = Schema.set schemas

    let messages =
        [ for (ns,_),wsdl in imports do
            yield! parseMessages (XNamespace.Get ns) wsdl schemaSet ]

    let portTypes =
        [ for (ns,_),wsdl in imports do
            yield! parsePortTypes (XNamespace.Get ns) wsdl messages ] 

    let bindings =
        [ for (ns,_), wsdl in imports do
            yield! parseBindings (XNamespace.Get ns) wsdl portTypes]

    let services = 
        [ for (ns,_), wsdl in imports do
            yield! parseServices bindings (XNamespace.Get ns) wsdl  ]

    saveSchema imports schemas

    { Schemas = schemaSet
      Services = services }

type XmlRelativeResolver(baseUri: Uri) =
    inherit XmlUrlResolver()

    override _.ResolveUri(_, uri) =

        let uri = Uri(uri, UriKind.RelativeOrAbsolute)
        if uri.IsAbsoluteUri then
            uri
        else
            Uri(baseUri, uri)
        
let parse (wsdl: XDocument) documentUri saveSchema =
    let resolver = XmlRelativeResolver(documentUri)
    parseWithLoader wsdl.Root documentUri resolver saveSchema

type DocUri =
    | XsdUri of string
    | WsdlUri of string

type XmlLocalResolver(files: Map<DocUri, byte[]>) =
    inherit XmlResolver()
    override this.GetEntity(uri, role, ofObjectToReturn) =
        let key = 
            match role with
            | "wsdl" -> WsdlUri (string uri)
            | _ -> XsdUri (string uri)
        let bytes = files.[key]
        new IO.MemoryStream(bytes) |> box
            
        // api.microsofttranslator.com.V2.wsdl
        //"http://api.microsofttranslator.com/V2/soap.svc?wsdl=wsdl0"

let parseWsdlSchema (schema: XDocument) documentUri =
    let root = 
        schema.Root.Elements()
        |> Seq.filter(fun e ->  e.Descendants(Wsdl.service) |> Seq.isEmpty |> not)
        |> Seq.map (fun e -> e.Element(Wsdl.ns + "definitions"))
        |> Seq.head

    let docs = 
        [ for f in schema.Root.Elements() -> 
            let name = f.Attribute(XName.Get "name").Value
            let element = 
                if name.EndsWith(".wsdl") then
                    f.Element(Wsdl.ns + "definitions")
                else
                    f.Element(Xsd.ns + "schema")
            let tns = element.Attribute(XName.Get "targetNamespace").Value

            use stream = new IO.MemoryStream()
            element.Save(stream)
            let bytes = stream.ToArray()
            if name.EndsWith(".wsdl") then
                WsdlUri tns, bytes
            else
                XsdUri tns, bytes
        ] |> Map.ofList

    let files = 
        [ for import in schema.Root.Descendants(Wsdl.ns + "import") ->
            let ns = import.Attribute(XName.Get "namespace").Value
                
            let location = 
                let l =  Uri(import.Attribute(XName.Get "location").Value, UriKind.RelativeOrAbsolute)
                if l.IsAbsoluteUri then
                    l
                else
                    Uri(documentUri, l)
            WsdlUri location.AbsoluteUri, docs.[WsdlUri ns]
       
          for import in schema.Root.Descendants(Xsd.ns + "import") ->
            let ns = import.Attribute(XName.Get "namespace").Value
            let location =
                let l = Uri(import.Attribute(XName.Get "schemaLocation").Value, UriKind.RelativeOrAbsolute)
                if l.IsAbsoluteUri then
                    l
                else
                    Uri(documentUri, l)
            XsdUri location.AbsoluteUri, docs.[XsdUri ns]
        ] 
        |> Map.ofList

    parseWithLoader root documentUri (XmlLocalResolver(files)) (fun _ _ -> ())

type PortOperation with
    member this.RequireContract =
        match this.Input with
        | None 
        | Some { Element = SimpleType _ }
        | Some { Element = Element { Type = InlineType (XsSimpleType _ ) }} 
        | Some { Element = Element { Type = InlineType (XsComplexType { Elements = NoContent }) } }
        | Some { Element = Element { Type = InlineType (XsComplexType { Elements = Sequence [ XsElement _ ] }) } }
            -> false
        | _ -> true
