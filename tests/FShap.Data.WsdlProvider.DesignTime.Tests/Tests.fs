[<NUnit.Framework.TestFixture>]
module FShap.Data.WsdlProvider.DesignTime.WsdlTest

#if INTERACTIVE
#r "System.Xml.Linq"
#load "../../src/FSharp.Data.WsdlProvider.DesignTime/Wsdl.fs"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../FSharp.Data.WsdlProvider.Tests/"
#endif

open System

open FSharp.Data.Wsdl
open System.Xml
open System.Xml.Schema
open System.Xml.Linq
open NUnit.Framework

let loadWsdl name = 
    XDocument.Load(name: string)
    |> parse 

[<SetUp>]
let setup() =
    let execDir = 
        Reflection.Assembly.GetExecutingAssembly().Location
        |> IO.Path.GetDirectoryName
    Environment.CurrentDirectory <- execDir

[<Test>]
let ``Weather Wsld loading should succeed`` () =
    let wsdl = loadWsdl "./Weather.wsdl"
    
    Assert.NotNull(wsdl)


[<Test>]
let ``Translator Wsld loading should succeed`` () =
    let wsdl = loadWsdl "./Translator.wsdl"
    //let wsdl = loadWsdl "http://api.microsofttranslator.com/V2/soap.svc"
    
    Assert.NotNull(wsdl)


[<Test>]
let ``Element can be lists (with min=0 and max=unbounded)`` () =
    let wsdl = loadWsdl "./Weather.wsdl"
    let ns = "http://ws.cdyne.com/WeatherWS/"

    let t = 
        wsdl.Schemas.GlobalTypes.[XmlQualifiedName("ArrayOfWeatherDescription", ns)]
        :?> XmlSchemaComplexType

    match t.Particle with
    | XsdSequence [ XsdElement e] -> 
        Assert.AreEqual(0m, e.MinOccurs)
        Assert.AreEqual(Decimal.MaxValue, e.MaxOccurs)

    | _ -> Assert.Fail("Cannot find element")



[<Test>]
let ``Element can have empty complex type`` () =
    // this is used for actions that take no inupt parameters
    let wsdl = loadWsdl "./Weather.wsdl"
    let ns = "http://ws.cdyne.com/WeatherWS/"

    let element = 
        wsdl.Schemas.GlobalElements.[XmlQualifiedName("GetWeatherInformation", ns)]
        :?> XmlSchemaElement

    match element.SchemaType with
    | XsdComplexType t -> 
        Assert.AreEqual(XmlSchemaContentType.Empty, t.ContentType)
     
    | _ -> Assert.Fail("Unexpected schema type")

[<Test>]
let ``ComplexType contains elements``() =
    let wsdl = loadWsdl "./Weather.wsdl"
       
    let ns = "http://ws.cdyne.com/WeatherWS/"
    let complexType = 
        wsdl.Schemas.GlobalTypes.[XmlQualifiedName("temp", ns)]
        :?> XmlSchemaComplexType
    let elements = 
        match complexType.Particle with
        | XsdSequence s ->
            s
            |> List.choose (function 
                | XsdElement e -> Some (e.Name, e.MinOccurs, e.MaxOccurs, e.ElementSchemaType.TypeCode)
                | _ -> None
                )
        | _ -> []

    Assert.AreEqual([ "MorningLow" , 0m, 1m, XmlTypeCode.String
                      "DaytimeHigh", 0m, 1m, XmlTypeCode.String ] , elements)
     

[<Test>]
let ``Services contains declares services`` () =
    // this is used for actions that take no inupt parameters
    let wsdl = loadWsdl "./Weather.wsdl"
    
    Assert.AreEqual(1, wsdl.Services.Length)
    Assert.AreEqual("Weather", wsdl.Services.[0].Name)


[<Test>]
let ``Services contains Soap ports `` () =
    // this is used for actions that take no inupt parameters
    let wsdl = loadWsdl "./Weather.wsdl"
    
    let service = wsdl.Services.[0]
    Assert.AreEqual(1, service.Ports.Length)
    Assert.AreEqual("WeatherSoap", service.Ports.[0].Name.LocalName )
    Assert.AreEqual("http://wsf.cdyne.com/WeatherWS/Weather.asmx", service.Ports.[0].Location )

