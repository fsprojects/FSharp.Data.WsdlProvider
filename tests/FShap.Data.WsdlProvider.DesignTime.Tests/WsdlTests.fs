[<NUnit.Framework.TestFixture>]
module FShap.Data.WsdlProvider.DesignTime.WsdlTests

#if INTERACTIVE
#r "System.Xml.Linq"
#load "../../src/FSharp.Data.WsdlProvider.DesignTime/Wsdl.fs"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../FSharp.Data.WsdlProvider.Tests/"
#endif

open System

open FSharp.Data.Wsdl
open FSharp.Data.Xsd
open System.Xml
open System.Xml.Schema
open System.Xml.Linq
open NUnit.Framework

let loadWsdl name = 
    let doc = XDocument.Load(name: string)
    parse doc (Uri(IO.Path.Combine(Environment.CurrentDirectory, name))) dontSave



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
let ``Synxis Wsld loading should succeed`` () =
    let wsdl = loadWsdl "./Synxis.wsdl"
    
    Assert.NotNull(wsdl)

[<Test>]
let ``Translator Wsld loading should succeed`` () =
    let wsdl = loadWsdl "./Translator.wsdl"
    //let wsdl = loadWsdl "http://api.microsofttranslator.com/V2/soap.svc"
    
    Assert.NotNull(wsdl)

[<Test>]
let ``Planning Wsld loading should succeed`` () =
    let wsdl = loadWsdl "./Planning.wsdl"
    
    Assert.NotNull(wsdl)



[<Test>]
let ``Element can be lists (with min=0 and max=unbounded)`` () =
    let wsdl = loadWsdl "./Weather.wsdl"
    let ns = XNamespace.Get "http://ws.cdyne.com/WeatherWS/"

    let t = 
        wsdl.Schemas.Types.[ ns + "ArrayOfWeatherDescription"] 

    let expected =
        { Name = ns + "ArrayOfWeatherDescription"
          Type = 
            XsComplexType
                { XsType.empty with
                    Elements = 
                        Sequence [ 
                            XsElement 
                                { Name = ns + "WeatherDescription"
                                  Type = TypeRef (ns + "WeatherDescription")
                                  Occurs = { Min = MinOccurs 0; Max = Unbounded }
                                  DefaultValue = None 
                                  SubstitutionGroup = None } ]
                }
        }
    Assert.AreEqual(expected, t)



[<Test>]
let ``Element can have empty complex type`` () =
    // this is used for actions that take no inupt parameters
    let wsdl = loadWsdl "./Weather.wsdl" 
    let ns = XNamespace.Get "http://ws.cdyne.com/WeatherWS/"

    let element = 
        wsdl.Schemas.Elements.[ns + "GetWeatherInformation"]

    let expected =
        { Name = ns + "GetWeatherInformation"
          Type = InlineType (XsComplexType XsType.empty)
          Occurs = Occurs.once
          DefaultValue = None
          SubstitutionGroup = None }
    Assert.AreEqual(expected, element)
     

[<Test>]
let ``ComplexType contains elements``() =
    let wsdl = loadWsdl "./Weather.wsdl"
       
    let ns = XNamespace.Get "http://ws.cdyne.com/WeatherWS/"
    let xs = XNamespace.Get "http://www.w3.org/2001/XMLSchema"
    let complexType = 
        wsdl.Schemas.Types.[ns + "temp"]
    let expected = 
        { Name = ns + "temp"
          Type =
            XsComplexType
                { XsType.empty with
                    Elements = 
                        Sequence [ XsElement 
                                    { Name = ns + "MorningLow"
                                      Type = TypeRef(xs + "string")
                                      Occurs = Occurs.optional
                                      DefaultValue = None
                                      SubstitutionGroup = None }
                                   XsElement 
                                    { Name = ns + "DaytimeHigh"
                                      Type = TypeRef(xs + "string")
                                      Occurs = Occurs.optional
                                      DefaultValue = None
                                      SubstitutionGroup = None }]}
        }

    Assert.AreEqual(expected , complexType)
     

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


[<Test>]
let ``Document style wsdl is detected`` () =
    // this is used for actions that take no inupt parameters
    let wsdl = loadWsdl "./Eutax.wsdl"
    
    let service = wsdl.Services.[0]
    let binding = service.Ports.[0].Binding
    Assert.AreEqual(Document, binding.Style)



[<Test>]
let ``Local schema is writen correctly`` () =
    // this is used for actions that take no inupt parameters
    let fullPath = IO.Path.Combine(Environment.CurrentDirectory,"./Translator.wsdl" )
    let wsdl = XDocument.Load(fullPath)
    let writer = new IO.StringWriter()
    let _ = parse wsdl (Uri fullPath) (writeLocalSchema writer)

    let doc = XDocument.Parse(writer.ToString())
    Assert.AreEqual(XName.Get "ServiceMetadataFiles", doc.Root.Name)
    let names = 
        [ "api.microsofttranslator.com.V2.wsdl"
          "tempuri.org.wsdl"
          "api.microsofttranslator.com.V2.xsd"
          "Microsoft.MT.Web.Service.V2.xsd"
          "schemas.microsoft.com.2003.10.Serialization.Arrays.xsd"
          "schemas.microsoft.com.2003.10.Serialization.xsd" ]

    CollectionAssert.AreEquivalent(names, doc.Root.Elements( XName.Get "ServiceMetadataFile").Attributes(XName.Get "name") |> Seq.map (fun a -> a.Value) )


    
[<Test>]
let ``Local schema can roundtrip`` () =
    // this is used for actions that take no inupt parameters
    let fullPath = IO.Path.Combine(Environment.CurrentDirectory,"./Translator.wsdl" )
    let wsdl = XDocument.Load(fullPath)
    let writer = new IO.StringWriter()
    let parsedWsdl = parse wsdl (Uri fullPath) (writeLocalSchema writer)

    let localSchema = XDocument.Parse(writer.ToString())
    let localfullPath = IO.Path.Combine(Environment.CurrentDirectory, "Translator.wsdlschema")

    let parsedLocalWsdl = parseWsdlSchema localSchema (Uri localfullPath)

    Assert.AreEqual(parsedWsdl, parsedLocalWsdl)
