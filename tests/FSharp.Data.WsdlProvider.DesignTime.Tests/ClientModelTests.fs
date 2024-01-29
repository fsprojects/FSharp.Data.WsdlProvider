[<NUnit.Framework.TestFixture>]
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
                             Members = [ CTChild.CTArrayChoice( [CTElement("property", (XNamespace.op_Implicit "http://ws.availpro.com/internal/schemas/planning/2012A") + "property", TRef "ChangePropertyResult",0)
                                                                 CTElement("entity", (XNamespace.op_Implicit "http://ws.availpro.com/internal/schemas/planning/2012A") + "entity", TRef "ChangeEntityResult",0)], 0) ] }
    
    Assert.AreEqual(expected, testResponse)


[<Test>]
let ``ChangeSetResponse.completionDate should be optional `` () =
    // this wsdl first failed to load because of the choice element type
    let wsdl = loadClientModel "ChangeSetService.wsdl"
    let testResponse = wsdl.Types |> Seq.find( fun t -> t.TypeName = "ChangeSetResponse")

    match testResponse with
    | TypeDef.ComplexType ct ->
        let found = ct.Members |> List.exists(function CTAttribute("completionDate",_,_,true) -> true | _ -> false)
        Assert.IsTrue(found)
    | _ -> failwith "ChangeSetResponse should be a complex type"
        

let singleChoice =
    """
    <wsdl:definitions xmlns:s="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://schemas.xmlsoap.org/wsdl/soap12/" xmlns:http="http://schemas.xmlsoap.org/wsdl/http/" xmlns:mime="http://schemas.xmlsoap.org/wsdl/mime/" xmlns:tns="http://ws.availpro.com/internal/schemas/planning/2012A" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:tm="http://microsoft.com/wsdl/mime/textMatching/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" targetNamespace="http://ws.availpro.com/internal/schemas/planning/2012A" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/">
      <wsdl:types>
        <s:schema elementFormDefault="qualified" targetNamespace="http://ws.availpro.com/internal/schemas/planning/2012A">
          <s:element name="GetChangeSet">
            <s:complexType>
              <s:sequence>
                <s:element minOccurs="1" maxOccurs="1" name="hotelId" type="s:int" />
                <s:element minOccurs="0" maxOccurs="1" name="changeSetId" type="s:string" />
              </s:sequence>
            </s:complexType>
          </s:element>
          <s:element name="GetChangeSetResponse">
            <s:complexType>
              <s:sequence>
                <s:element minOccurs="0" maxOccurs="1" name="GetChangeSetResult" type="tns:PlanningResponseMessage" />
              </s:sequence>
            </s:complexType>
          </s:element>
          <s:element name="metadata" nillable="true" type="tns:Metadata" />
          <s:complexType name="Metadata">
            <s:sequence>
              <s:element minOccurs="1" maxOccurs="1" name="application">
                <s:complexType>
                  <s:attribute name="name" type="s:string" use="required" />
                </s:complexType>
              </s:element>
            </s:sequence>
            <s:attribute name="groupId" type="s:unsignedInt" use="required" />
            <s:attribute name="hotelId" type="s:unsignedInt" use="required" />
            <s:attribute name="userId" type="s:unsignedInt" use="optional" />
          </s:complexType>
          <s:simpleType name="ChangeStatus">
            <s:restriction base="s:string">
              <s:enumeration value="None" />
              <s:enumeration value="Pending" />
              <s:enumeration value="Success" />
              <s:enumeration value="Warning" />
              <s:enumeration value="Failure" />
            </s:restriction>
          </s:simpleType>
          <s:complexType name="Error">
            <s:sequence>
              <s:element minOccurs="0" maxOccurs="1" name="comment" type="s:string" />
            </s:sequence>
            <s:attribute name="type" type="s:string" use="required" />
            <s:attribute name="message" type="s:string" use="required" />
          </s:complexType>
          <s:complexType name="ChangeSetResponse">
            <s:sequence>
              <s:element minOccurs="1" maxOccurs="1" name="metadata" type="tns:Metadata" />
            </s:sequence>
            <s:attribute name="requestDate" type="s:dateTime" use="required" />
            <s:attribute name="completionDate" type="s:dateTime" use="optional" />
            <s:attribute name="status" type="tns:ChangeStatus" use="required" />
            <s:attribute default="false" name="ignoreFailures" type="s:boolean" use="optional" />
            <s:attribute default="false" name="legacyReload" type="s:boolean" use="optional" />
          </s:complexType>
          <s:complexType name="PlanningResponseMessage">
            <s:sequence>
              <s:element minOccurs="0" maxOccurs="1" name="success" />
              <s:element minOccurs="0" maxOccurs="1" name="warning" type="tns:Error" />
              <s:element minOccurs="0" maxOccurs="1" name="failure" type="tns:Error" />
              <s:choice minOccurs="0" maxOccurs="1">
                <s:element minOccurs="0" maxOccurs="1" name="changeSetResponse" type="tns:ChangeSetResponse" />
              </s:choice>
            </s:sequence>
          </s:complexType>
        </s:schema>
      </wsdl:types>
      <wsdl:message name="GetChangeSetSoapIn">
        <wsdl:part name="parameters" element="tns:GetChangeSet" />
      </wsdl:message>
      <wsdl:message name="GetChangeSetSoapOut">
        <wsdl:part name="parameters" element="tns:GetChangeSetResponse" />
      </wsdl:message>
      <wsdl:portType name="ChangeSetService2012ASoap">
        <wsdl:operation name="GetChangeSet">
          <wsdl:documentation xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/">Gets the result of a change set.</wsdl:documentation>
          <wsdl:input message="tns:GetChangeSetSoapIn" />
          <wsdl:output message="tns:GetChangeSetSoapOut" />
        </wsdl:operation>
      </wsdl:portType>
      <wsdl:binding name="ChangeSetService2012ASoap" type="tns:ChangeSetService2012ASoap">
        <soap:binding transport="http://schemas.xmlsoap.org/soap/http" />
        <wsdl:operation name="GetChangeSet">
          <soap:operation soapAction="http://ws.availpro.com/internal/schemas/planning/2012A/GetChangeSet" style="document" />
          <wsdl:input>
            <soap:body use="literal" />
          </wsdl:input>
          <wsdl:output>
            <soap:body use="literal" />
          </wsdl:output>
        </wsdl:operation>
      </wsdl:binding>
      <wsdl:service name="ChangeSetService2012A">
        <wsdl:port name="ChangeSetService2012ASoap" binding="tns:ChangeSetService2012ASoap">
          <soap:address location="http://planning.internal-services.production.siriona.com/planning/2012A/changesetservice.asmx" />
        </wsdl:port>
      </wsdl:service>
    </wsdl:definitions>
    """


[<Test>]
let ``Single case choice should not have obj type`` () =
    let model =
        let wsdl = parse (XDocument.Parse singleChoice) (Uri "http://sample.com") dontSave
        createModel wsdl
    let testResponse = model.Types |> Seq.find( fun t -> t.TypeName = "PlanningResponseMessage")


    match testResponse with
    | TypeDef.ComplexType ct ->
       
        let changeSetResponse = ct.Members |> List.tryFind (function CTChild.CTElement("changeSetResponse", _,_, _) -> true | _ -> false )
        match changeSetResponse with
        | Some changeSetResponse ->
            Assert.AreEqual("ChangeSetResponse",changeSetResponse.TypeName)
        | None ->
            failwith "changeSetResponse property not found because single choice was not reduced"
    | _ -> failwith "Invalid PlanningResponseMessage type"


