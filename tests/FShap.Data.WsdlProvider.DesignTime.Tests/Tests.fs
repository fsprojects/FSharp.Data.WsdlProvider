[<NUnit.Framework.TestFixture>]
module FShap.Data.WsdlProvider.DesignTime.WsdlTest

#if INTERACTIVE
#r "System.Xml.Linq"
#load "../../src/FSharp.Data.WsdlProvider.DesignTime/Wsdl.fs"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../FSharp.Data.WsdlProvider.Tests/"
#endif

open System

open FSharp.Data.Wsdl
open System.Xml.Linq
open NUnit.Framework

let findElement name typ =
    match typ with
    | Element e when e.Name = name -> Some e
    | _ -> None

let findComplexType name typ =
    match typ with
    | ComplexType ct when ct.Name = Some name -> Some ct
    | _ -> None

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
let ``Wsld loading should succeed`` () =
    let wsdl = loadWsdl "./Weather.wsdl"
    
    Assert.NotNull(wsdl)

[<Test>]
let ``Target namespace should be read from file`` () =
    let wsdl = loadWsdl "./Weather.wsdl"
    
    Assert.AreEqual("http://ws.cdyne.com/WeatherWS/", wsdl.TargetNamespace)


[<Test>]
let ``Element can be lists (with min=0 and max=unbounded)`` () =
    let wsdl = loadWsdl "./Weather.wsdl"

    let element = 
        wsdl.Types
        |> List.pick (findElement "GetWeatherInformation")


    Assert.AreEqual(Occurs 0, element.MinOccurs)
    Assert.AreEqual(Unbounded, element.MaxOccurs)


[<Test>]
let ``Element can have empty complex type`` () =
    // this is used for actions that take no inupt parameters
    let wsdl = loadWsdl "./Weather.wsdl"
    
    let element = 
        wsdl.Types
        |> List.pick (findElement "GetWeatherInformation")

    Assert.AreEqual(ComplexType { Name = None; Elements = [] }, element.Type)

let ``ComplexType contains elements``() =
    let wsdl = loadWsdl "./Weather.wsdl"
       
    let complexType = 
        wsdl.Types
        |> List.pick (findComplexType "temp")

    Assert.AreEqual([{ MinOccurs = Occurs 0
                       MaxOccurs = Occurs 1
                       Name = "MorningLow"
                       Type = Primitive XsdString }
                     { MinOccurs = Occurs 0
                       MaxOccurs = Occurs 1
                       Name = "DaytimeHigh"
                       Type = Primitive XsdString } 
                       ], complexType.Elements)
    

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
    Assert.AreEqual("WeatherSoap", service.Ports.[0].Name )
    Assert.AreEqual("http://wsf.cdyne.com/WeatherWS/Weather.asmx", service.Ports.[0].Location )

