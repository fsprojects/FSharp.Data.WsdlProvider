
This is an implementation of the Wsdl type provider that is compatible with netcore and netfx.

It does all the wsdl parsing internally and doesn't require an external tool.


Building:

    dotnet tool restore
    dotnet paket update
    dotnet build -c release

    dotnet paket pack nuget --version 0.0.1
