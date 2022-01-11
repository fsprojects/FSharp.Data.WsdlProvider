# FSharp.Data.WsdlProvider

This is an implementation of the F# Wsdl type provider that is compatible with netcore and netfx.

It does all the wsdl parsing internally and doesn't require an external tool.

![Build and Test](https://github.com/thinkbeforecoding/FSharp.Data.WsdlProvider/workflows/Build%20and%20Test/badge.svg)

## Quickstart

Add the nuget to your project from [nuget.org](https://www.nuget.org/packages/FSharp.Data.WsdlProvider/):

    dotnet add .\Client\Client.fsproj package FSharp.Data.WsdlProvider --prerelease

Define a type using the WsdlProvider pointing at you wsdl definition:

    open FSharp.Data
    type MyService = WsdlProvider<"https://mysite.com/service.wsld">

Call the service:

    use client = MyService.ServiceSoapClient("https://mysite.com/service")
    let result = client.MyMethod("arg")
    printfn "%A" result

## Build

To build the type provider, you need only to have [dotnet sdk 6.0 installed](https://dotnet.microsoft.com/download).

Building on Windows:

    .\build.cmd

Binding on Minux/MacOS:
    
    ./build.sh



