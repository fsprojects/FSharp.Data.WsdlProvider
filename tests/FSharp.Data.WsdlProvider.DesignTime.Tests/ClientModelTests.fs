﻿[<NUnit.Framework.TestFixture>]
module FSharp.Data.WsdlProvider.DesignTime.ClientModelTests

#if INTERACTIVE
#r "System.Xml.Linq"
#load "../../src/FSharp.Data.WsdlProvider.DesignTime/Wsdl.fs"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../FSharp.Data.WsdlProvider.Tests/"
#endif

open System

open FSharp.Data.Wsdl
open FSharp.Data.ClientModel
open System.Xml.Linq
open NUnit.Framework

let loadClientModel name = 
    let doc = XDocument.Load(name: string)
    let uri =
        match Uri.TryCreate(name, UriKind.Absolute) with
        | true, uri -> uri
        | false,_ ->
            Uri(IO.Path.Combine(Environment.CurrentDirectory, name))


    let wsdl = parse doc uri dontSave
    createModel wsdl



[<SetUp>]
let setup() =
    let execDir = 
        Reflection.Assembly.GetExecutingAssembly().Location
        |> IO.Path.GetDirectoryName
    Environment.CurrentDirectory <- execDir

[<Test>]
let ``Weather Wsdl loading should succeed`` () =
    let wsdl = loadClientModel "./Weather.wsdl"
    
    Assert.NotNull(wsdl)

[<Test>]
let ``Synxis Wsdl loading should succeed`` () =
    let wsdl = loadClientModel "./Synxis.wsdl"
    
    Assert.NotNull(wsdl)

[<Test>]
let ``Translator Wsdl loading should succeed`` () =
    let wsdl = loadClientModel "./Translator.wsdl"
    
    Assert.NotNull(wsdl)

[<Test>]
let ``Planning Wsdl loading should succeed`` () =
    let wsdl = loadClientModel "./Planning.wsdl"
    
    Assert.NotNull(wsdl)

[<Test>]
let ``Bug #6: NationalRail Wsdl loading should succeed`` () =
    let wsdl = loadClientModel "./NationalRail.wsdl"
    
    Assert.NotNull(wsdl)
    Assert.IsNotEmpty(wsdl.Services)

[<Test>]
let ``ChangeSetService Wsld loading should succeed`` () =
    let wsdl = loadClientModel "./ChangeSetService.wsdl"
    
    Assert.NotNull(wsdl)




[<Test>]
let ``Document style wsdl is detected`` () =
    // this is used for actions that take no inupt parameters
    let wsdl = loadClientModel "./Eutax.wsdl"

    Assert.NotNull(wsdl)
    




    
[<Test>]
let ``DirectClick`` () =
    // this wsdl first failed to load because of the lack of schemaLocation
    // for the http://schemas.xmlsoap.org/soap/encoding/ namespace,
    // then because of the presence of a XmlSchemaSimpleTypeRestriction
    let wsdl = loadClientModel "DirectClickService.wsdl"
    
    Assert.IsNotNull(wsdl)

    
[<Test>]
let ``Choice is loaded correctly`` () =
    // this wsdl first failed to load because of the choice element type
    let wsdl = loadClientModel "ChangeSetService.wsdl"
    
    Assert.IsNotNull(wsdl)

[<Test>]
let ``TestResponse is a contract`` () =
    // this wsdl first failed to load because of the choice element type
    let wsdl = loadClientModel "ChangeSetService.wsdl"
    let testResponse = wsdl.Types |> Seq.find( fun t -> t.TypeName = "TestResponse")
    let expected =
        TypeDef.Contract {ComplexTypeDef.TypeName = "TestResponse"
                          ComplexTypeDef.XmlName = (XNamespace.op_Implicit "http://ws.availpro.com/internal/schemas/planning/2012A") + "TestResponse"
                          ComplexTypeDef.Members = [] }
    
    Assert.AreEqual(expected, testResponse)



[<Test>]
let ``ChangeSetSearchResponse changeSet property should be an array`` () =
    // this wsdl first failed to load because of the choice element type
    let wsdl = loadClientModel "ChangeSetService.wsdl"
    let testResponse = wsdl.Types |> Seq.find( fun t -> t.TypeName = "ChangeSetSearchResponse")
    let expected =
        TypeDef.ComplexType { TypeName = "ChangeSetSearchResponse"
                              XmlName = (XNamespace.op_Implicit "http://ws.availpro.com/internal/schemas/planning/2012A") + "ChangeSetSearchResponse"
                              Members = [ CTElement("changeSet", (XNamespace.op_Implicit "http://ws.availpro.com/internal/schemas/planning/2012A") + "changeSet", TRArray (TRef.TRef "ChangeSetSearchResponseChangeSet"), 0 ) ] }
    
    Assert.AreEqual(expected, testResponse)


[<Test>]
let ``ChangePropertyResultChangeDependencies items property should be an array`` () =
    // this wsdl first failed to load because of the choice element type
    let wsdl = loadClientModel "ChangeSetService.wsdl"
    let testResponse = wsdl.Types |> Seq.find( fun t -> t.TypeName = "ChangePropertyResultChangeDependencies")
    let expected =
        TypeDef.NoNameType { TypeName = "ChangePropertyResultChangeDependencies"
                             XmlName = null
                             Members = [ CTChild.CTArrayChoice( [CTElement("property", (XNamespace.op_Implicit "http://ws.availpro.com/internal/schemas/planning/2012A") + "property", TRArray (TRef "ChangePropertyResult"),0)
                                                                 CTElement("entity", (XNamespace.op_Implicit "http://ws.availpro.com/internal/schemas/planning/2012A") + "entity", TRArray (TRef "ChangeEntityResult"),0)], 0) ] }
    
    Assert.AreEqual(expected, testResponse)


