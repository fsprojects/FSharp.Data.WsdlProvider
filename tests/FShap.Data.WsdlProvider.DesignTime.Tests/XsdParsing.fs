module XsdParsing

open System
open System.Xml
open System.Xml.Schema
open System.Xml.Linq
open FSharp.Data.Wsdl
open FSharp.Data.Xsd
open NUnit.Framework


let loadSchema schema =
    let set = XmlSchemaSet()
    use reader = new IO.StringReader(schema)
    use xml = new Xml.XmlTextReader(reader)
    set.Add(null, xml) |> ignore
    set.Compile()
    set


let dedge = XNamespace.Get "https://d-edge.com"
let xs = XNamespace.Get "http://www.w3.org/2001/XMLSchema"


[<Test>]
let ``Simple type element``() =
    let xsd = 
        """
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                targetNamespace="https://d-edge.com"
                xmlns:tns="https://d-edge.com"
                elementFormDefault="qualified">
            <xs:element name="Simple" type="xs:string"/>
        </xs:schema>
        """
        |> loadSchema

    let e = Schema.element (dedge + "Simple") xsd

    let expected = 
          { Name = dedge + "Simple"
            Type = TypeRef (xs + "string")
            DefaultValue = None
            Occurs = Occurs.once
            SubstitutionGroup = None }
          
    Assert.AreEqual(expected, e )
        

[<Test>]
let ``Simple type element support default value``() =
    let xsd = 
        """
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                targetNamespace="https://d-edge.com"
                xmlns:tns="https://d-edge.com"
                elementFormDefault="qualified">
            <xs:element name="Simple" type="xs:integer" default="42"/>
        </xs:schema>
        """
        |> loadSchema

    let e = Schema.element (dedge + "Simple") xsd

    let expected = 
          { Name = dedge + "Simple"
            Type = TypeRef (xs + "integer")
            DefaultValue = Some "42"
            Occurs = Occurs.once
            SubstitutionGroup = None }
          
    Assert.AreEqual(expected, e )
        
[<Test>]
let ``Simple type element support fixed value as default``() =
    let xsd = 
        """
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                targetNamespace="https://d-edge.com"
                xmlns:tns="https://d-edge.com"
                elementFormDefault="qualified">
            <xs:element name="Simple" type="xs:integer" fixed="42"/>
        </xs:schema>
        """
        |> loadSchema

    let e = Schema.element (dedge + "Simple") xsd

    let expected = 
          { Name = dedge + "Simple"
            Type = TypeRef (xs + "integer")
            DefaultValue = Some "42"
            Occurs = Occurs.once
            SubstitutionGroup = None} 
          
    Assert.AreEqual(expected, e )
        

[<Test>]
let ``Typeref element``() =
    let xsd = 
        """
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                targetNamespace="https://d-edge.com"
                xmlns:tns="https://d-edge.com"
                elementFormDefault="qualified">
            <xs:complexType name="product">
              <xs:attribute name="prodid" type="xs:positiveInteger"/>
            </xs:complexType>
            <xs:element name="Ref" type="tns:product"/>
        </xs:schema>
        """
        |> loadSchema

    let e = Schema.element (dedge + "Ref") xsd

    let expected = 
        { Name = dedge + "Ref"
          Type = TypeRef (dedge + "product")
          DefaultValue = None
          Occurs = Occurs.once
          SubstitutionGroup = None }
        
    Assert.AreEqual(expected, e )

[<Test>]
let ``ComplexType element``() =
    let xsd = 
        """
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                targetNamespace="https://d-edge.com"
                xmlns:tns="https://d-edge.com"
                elementFormDefault="qualified">
            <xs:element name="Complex">
                <xs:complexType />
            </xs:element>
        </xs:schema>
        """
        |> loadSchema
    let expected = 
        { Name = dedge + "Complex"
          Type = InlineType (XsComplexType XsType.empty)
          DefaultValue = None
          Occurs = Occurs.once
          SubstitutionGroup = None }
    
    let e = Schema.element (dedge + "Complex") xsd 
    Assert.AreEqual(expected , e)

[<Test>]
let ``ComplexType element with attribute element``() =
    let xsd = 
        """
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                targetNamespace="https://d-edge.com"
                xmlns:tns="https://d-edge.com"
                elementFormDefault="qualified">
            <xs:element name="WithAttribute">
                <xs:complexType>
                  <xs:attribute name="prodid" type="xs:positiveInteger"/>
                </xs:complexType>
            </xs:element>
        </xs:schema>
        """
        |> loadSchema
    let expected = 
        { Name = dedge + "WithAttribute"
          Type = 
            InlineType 
             (XsComplexType
                { XsType.empty with
                      Attributes = 
                        [{ Name = XName.Get "prodid"; Type = XsSimple (xs + "positiveInteger"); DefaultValue = None }] })
          DefaultValue = None
          Occurs = Occurs.once
          SubstitutionGroup = None}
    
    let e = Schema.element (dedge + "WithAttribute") xsd 
    Assert.AreEqual(expected , e)
        
       
[<Test>]
let ``ComplexType element with elements``() =
    let xsd = 
        """
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                targetNamespace="https://d-edge.com"
                xmlns:tns="https://d-edge.com"
                elementFormDefault="qualified">
            <xs:element name="employee">
                <xs:complexType>
                  <xs:sequence>
                    <xs:element name="firstname" type="xs:string"/>
                    <xs:element name="lastname" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
            </xs:element>
        </xs:schema>
        """
        |> loadSchema
    let expected = 
        { Name = dedge + "employee"
          Type = 
            InlineType 
             ( XsComplexType
                { XsType.empty with
                      Elements = 
                        Sequence [ XsElement { Name = dedge + "firstname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None}
                                   XsElement { Name = dedge + "lastname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None}] } )
          DefaultValue = None
          Occurs = Occurs.once
          SubstitutionGroup = None } : XsElement 
    
    let e = Schema.element (dedge + "employee") xsd 
    Assert.AreEqual(expected , e)
        

[<Test>]
let ``ComplexType definition and element``() =
    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="employee" type="tns:personinfo"/>
        
        <xs:complexType name="personinfo">
          <xs:sequence>
            <xs:element name="firstname" type="xs:string"/>
            <xs:element name="lastname" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
     </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "employee"
       Type = TypeRef (dedge + "personinfo") 
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None}
    let expectedType =
        { Name = dedge + "personinfo"
          Type =        
            XsComplexType
             { XsType.empty with
                Elements = 
                    Sequence[ XsElement { Name = dedge + "firstname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None}
                              XsElement { Name = dedge + "lastname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None}] } }
        
    let e = Schema.element (dedge + "employee") xsd 
    let t = Schema.typeDef (dedge + "personinfo") xsd 
    Assert.AreEqual(expectedElement , e)
    Assert.AreEqual(expectedType , t)
 

[<Test>]       
let ``ComplexType with base type``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="employee" type="tns:fullpersoninfo"/>

        <xs:complexType name="personinfo">
          <xs:sequence>
            <xs:element name="firstname" type="xs:string"/>
            <xs:element name="lastname" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>

        <xs:complexType name="fullpersoninfo">
          <xs:complexContent>
            <xs:extension base="tns:personinfo">
              <xs:sequence>
                <xs:element name="address" type="xs:string"/>
                <xs:element name="city" type="xs:string"/>
                <xs:element name="country" type="xs:string"/>
              </xs:sequence>
            </xs:extension>
          </xs:complexContent>
        </xs:complexType>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "employee"
       Type = TypeRef (dedge + "fullpersoninfo") 
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None } 
    let expectedType =
        { Name = dedge + "fullpersoninfo"
          Type =
            XsComplexType
             { XsType.empty with
                BaseType = Some (dedge + "personinfo")
                Elements =
                    Sequence [ XsElement { Name = dedge + "firstname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None}
                               XsElement { Name = dedge + "lastname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None}
                               XsElement { Name = dedge + "address"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None}
                               XsElement { Name = dedge + "city"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None}
                               XsElement { Name = dedge + "country"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None} ] } } 
    let e = Schema.element (dedge + "employee") xsd 
    let t = Schema.typeDef (dedge + "fullpersoninfo") xsd 
    Assert.AreEqual(expectedElement , e)
    Assert.AreEqual(expectedType , t)


[<Test>]       
let ``ComplexType with base simple type``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="shoesize">
            <xs:complexType>
              <xs:simpleContent>
                <xs:extension base="xs:integer">
                  <xs:attribute name="country" type="xs:string" />
                </xs:extension>
              </xs:simpleContent>
            </xs:complexType>
        </xs:element>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "shoesize"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                BaseType = Some (xs + "integer")
                Attributes = [ { Name = XName.Get "country"; Type = XsSimple (xs + "string"); DefaultValue = None} ] })
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None } 
        
    let e = Schema.element (dedge + "shoesize") xsd 
    Assert.AreEqual(expectedElement , e)

[<Test>]       
let ``ComplexType with mixed``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
       <xs:element name="letter">
           <xs:complexType mixed="true">
             <xs:sequence>
               <xs:element name="name" type="xs:string"/>
               <xs:element name="orderid" type="xs:positiveInteger"/>
               <xs:element name="shipdate" type="xs:date"/>
             </xs:sequence>
           </xs:complexType>
        </xs:element>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "letter"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                Mixed = true
                Elements = 
                    Sequence [ XsElement { Name = dedge + "name"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                               XsElement { Name = dedge + "orderid"; Type = TypeRef (xs + "positiveInteger"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                               XsElement { Name = dedge + "shipdate"; Type = TypeRef (xs + "date"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } ] })

       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "letter") xsd 
    Assert.AreEqual(expectedElement , e)




[<Test>]       
let ``ComplexType with all``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="person">
            <xs:complexType>
              <xs:all>
                <xs:element name="firstname" type="xs:string"/>
                <xs:element name="lastname" type="xs:string"/>
              </xs:all>
            </xs:complexType>
        </xs:element>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "person"
       Type =
        InlineType
            (XsComplexType
                { XsType.empty with
                      Elements = All [ XsElement { Name = dedge + "firstname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                                       XsElement { Name = dedge + "lastname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } ] })

       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "person") xsd 
    Assert.AreEqual(expectedElement , e)

[<Test>]       
let ``ComplexType with choice``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="person">
            <xs:complexType>
              <xs:choice>
                <xs:element name="employee" type="xs:string"/>
                <xs:element name="member" type="xs:string"/>
              </xs:choice>
            </xs:complexType>
        </xs:element>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "person"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                  Elements = 
                    Choice [ XsElement { Name = dedge + "employee"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                             XsElement { Name = dedge + "member"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } ] })
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "person") xsd 
    Assert.AreEqual(expectedElement , e)

[<Test>]       
let ``ComplexType with minoccurs/maxoccurs``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="person">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="full_name" type="xs:string"/>
                <xs:element name="child_name" type="xs:string"
                maxOccurs="10" minOccurs="0"/>
              </xs:sequence>
            </xs:complexType>
        </xs:element>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "person"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                  Elements = 
                    Sequence [ XsElement { Name = dedge + "full_name"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                               XsElement { Name = dedge + "child_name"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = { Min = MinOccurs 0; Max = MaxOccurs 10 }; SubstitutionGroup = None } ] })

       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "person") xsd 
    Assert.AreEqual(expectedElement , e)


[<Test>]       
let ``ComplexType with minoccurs/ unbounded maxoccurs``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="person">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="full_name" type="xs:string"/>
                <xs:element name="child_name" type="xs:string"
                maxOccurs="unbounded" minOccurs="0"/>
              </xs:sequence>
            </xs:complexType>
        </xs:element>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "person"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                  Elements = 
                    Sequence [ XsElement { Name = dedge + "full_name"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                               XsElement { Name = dedge + "child_name"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = { Min = MinOccurs 0; Max = Unbounded }; SubstitutionGroup = None } ] })

       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "person") xsd 
    Assert.AreEqual(expectedElement , e)

[<Test>]       
let ``ComplexType with element group``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:group name="persongroup">
            <xs:sequence>
              <xs:element name="firstname" type="xs:string"/>
              <xs:element name="lastname" type="xs:string"/>
              <xs:element name="birthday" type="xs:date"/>
            </xs:sequence>
        </xs:group>

        <xs:element name="person">
            <xs:complexType>
                <xs:sequence>
                  <xs:group ref="tns:persongroup"/>
                  <xs:element name="country" type="xs:string"/>
                </xs:sequence>
            </xs:complexType>
        </xs:element>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "person"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                  Elements = 
                    Sequence [ XsElement { Name = dedge + "firstname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                               XsElement { Name = dedge + "lastname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None }
                               XsElement { Name = dedge + "birthday"; Type = TypeRef (xs + "date"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None }
                               XsElement { Name = dedge + "country"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } ] })

       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "person") xsd 
    Assert.AreEqual(expectedElement , e)

[<Test>]       
let ``ComplexType with attribute group``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:attributeGroup name="personattrgroup">
            <xs:attribute name="firstname" type="xs:string"/>
            <xs:attribute name="lastname" type="xs:string"/>
            <xs:attribute name="birthday" type="xs:date"/>
        </xs:attributeGroup>

        <xs:element name="person">
            <xs:complexType>
              <xs:attributeGroup ref="tns:personattrgroup"/>
              <xs:attribute name="country" type="xs:string"/>
            </xs:complexType>
        </xs:element>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "person"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                  Attributes = [ { Name = XName.Get "firstname"; Type = XsSimple(xs + "string"); DefaultValue = None} 
                                 { Name = XName.Get "lastname"; Type = XsSimple(xs + "string"); DefaultValue = None}
                                 { Name = XName.Get "birthday"; Type = XsSimple(xs + "date"); DefaultValue = None; }
                                 { Name = XName.Get "country"; Type = XsSimple(xs + "string"); DefaultValue = None} ] })

       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "person") xsd 
    Assert.AreEqual(expectedElement , e)

[<Test>]       
let ``ComplexType with anyattribute``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="person">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="firstname" type="xs:string"/>
                <xs:element name="lastname" type="xs:string"/>
              </xs:sequence>
              <xs:anyAttribute/>
            </xs:complexType>
        </xs:element>
    </xs:schema>
     """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "person"
       Type = InlineType
        (XsComplexType
             { XsType.empty with
                  Elements = 
                    Sequence [ XsElement { Name = dedge + "firstname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                               XsElement { Name = dedge + "lastname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } ]
                  AnyAttribute = true })

       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "person") xsd 
    Assert.AreEqual(expectedElement , e)

[<Test>]       
let ``ComplexType with any element``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="person">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="firstname" type="xs:string"/>
                <xs:element name="lastname" type="xs:string"/>
                <xs:any minOccurs="0"/>
              </xs:sequence>
            </xs:complexType>
        </xs:element>
    </xs:schema>
    """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "person"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                  Elements = 
                    Sequence [ XsElement { Name = dedge + "firstname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                               XsElement { Name = dedge + "lastname"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None } 
                               XsAny { Min = MinOccurs 0; Max = MaxOccurs 1 } 
                               ] })
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "person") xsd 
    Assert.AreEqual(expectedElement , e)

[<Test>]       
let ``ComplexType with substitution``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="name" type="xs:string"/>
        <xs:element name="navn" substitutionGroup="tns:name"/>


        <xs:element name="customer">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="tns:name"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>

        <xs:element name="kunde" substitutionGroup="tns:customer"/>
    </xs:schema>
    """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "customer"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                  Elements = 
                    Sequence [ XsElement { Name = dedge + "name"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None }  ]
                  })
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "customer") xsd 
    Assert.AreEqual(expectedElement , e)

    let expectedElement' = 
     { Name = dedge + "kunde"
       Type = InlineType
        (XsComplexType
            { XsType.empty with
                  Elements = 
                    Sequence [ XsElement { Name = dedge + "name"; Type = TypeRef (xs + "string"); DefaultValue = None; Occurs = Occurs.once; SubstitutionGroup = None  }  ]
                  })
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = Some (dedge + "customer") }
        
    let e' = Schema.element (dedge + "kunde") xsd 
    Assert.AreEqual(expectedElement' , e')



[<Test>]       
let ``SimpleType with restriction``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="age">
            <xs:simpleType>
              <xs:restriction base="xs:integer">
                <xs:minInclusive value="0"/>
                <xs:maxInclusive value="120"/>
              </xs:restriction>
            </xs:simpleType>
        </xs:element>
    </xs:schema>
    """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "age"
       Type = InlineType (XsSimpleType { BaseType = xs + "integer"; Enumeration = [] })
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "age") xsd 
    Assert.AreEqual(expectedElement , e)


[<Test>]       
let ``SimpleType with list``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="persons">
            <xs:complexType>
            <xs:attribute name="Ages">
              <xs:simpleType>
                <xs:list itemType="xs:string"/>
              </xs:simpleType>
            </xs:attribute>
      </xs:complexType>
        </xs:element>
    </xs:schema>
    """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "persons"
       Type = InlineType (XsComplexType { XsType.empty with Attributes = [ { Name = XName.Get "Ages"; Type = XsList (xs + "string"); DefaultValue = None} ] })
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "persons") xsd 
    Assert.AreEqual(expectedElement , e)


[<Test>]       
let ``SimpleType with enumeration``() =

    let xsd = 
     """
     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://d-edge.com"
             xmlns:tns="https://d-edge.com"
             elementFormDefault="qualified">
        <xs:element name="state">
            <xs:simpleType>
                <xs:annotation>
                    <xs:documentation>States in the Pacific Northwest of US</xs:documentation>
                </xs:annotation>
                <xs:restriction base="xs:string">
                  <xs:enumeration value='WA'>
                    <xs:annotation>
                      <xs:documentation>Washington</xs:documentation>
                    </xs:annotation>
                  </xs:enumeration>
                  <xs:enumeration value='OR'>
                    <xs:annotation>
                      <xs:documentation>Oregon</xs:documentation>
                    </xs:annotation>
                  </xs:enumeration>
                  <xs:enumeration value='ID'>
                    <xs:annotation>
                      <xs:documentation>Idaho</xs:documentation>
                    </xs:annotation>
                  </xs:enumeration>
                </xs:restriction>
            </xs:simpleType>
        </xs:element>
    </xs:schema>
    """
    |> loadSchema
    let expectedElement = 
     { Name = dedge + "state"
       Type = InlineType (XsSimpleType { 
                BaseType = xs + "string"
                Enumeration = [ { Value = "WA"; Name = "Washington" }
                                { Value = "OR"; Name = "Oregon" }
                                { Value = "ID"; Name = "Idaho" } ] })
       DefaultValue = None
       Occurs = Occurs.once
       SubstitutionGroup = None }
        
    let e = Schema.element (dedge + "state") xsd 
    Assert.AreEqual(expectedElement , e)

