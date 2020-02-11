module FSharp.Data.WsdlProviderTests


open FSharp.Data.WsdlProvider
open NUnit.Framework
open System
open System.ServiceModel

type Weather = WsdlService<const(__SOURCE_DIRECTORY__ + "/Weather.wsdl")>

[<Test>]
let ``Client can be instanciated with binding and EndPoint Address `` () =
    use client = new Weather.WeatherSoapClient(BasicHttpBinding(), EndpointAddress("http://wsf.cdyne.com/WeatherWS/Weather.asmx"))
    Assert.IsNotNull(client)
    ()


[<Test>]
let ``Client can be instanciated with Address only`` () =
    use client = new Weather.WeatherSoapClient("http://someaddress.com")
    Assert.AreEqual(Uri "http://someaddress.com" , client.Endpoint.Address.Uri)
    ()


[<Test>]
let ``Client can be instanciated with default ctor`` () =
    use client = new Weather.WeatherSoapClient()
    Assert.AreEqual(Uri "http://wsf.cdyne.com/WeatherWS/Weather.asmx",client.Endpoint.Address.Uri )
    ()


[<Test>]
let ``Client implements Soap service interface`` () =
    use client = new Weather.WeatherSoapClient(BasicHttpBinding(), EndpointAddress("http://wsf.cdyne.com/WeatherWS/Weather.asmx"))
    let service = client :> Weather.WeatherSoap
    Assert.IsNotNull(service)
    ()



[<Test>]
let ``Soap client provides location as a static member`` () =
    //use client = new Weather.WeatherSoapClient(BasicHttpBinding(), EndpointAddress("http://example.com"))
    let location = Weather.WeatherSoapClient.Location


    Assert.AreEqual("http://wsf.cdyne.com/WeatherWS/Weather.asmx", location)
    ()


