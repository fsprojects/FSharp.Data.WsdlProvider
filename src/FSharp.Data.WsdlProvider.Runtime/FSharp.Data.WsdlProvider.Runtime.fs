namespace FSharp.Data

open System
open System.ServiceModel
open System.ServiceModel.Channels

type DefaultBinding = 
    static member SelectBinding(uri: string) =
        if (Uri uri).Scheme = Uri.UriSchemeHttps then
            BasicHttpsBinding() :> Binding
        else
            BasicHttpBinding() :> Binding


// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("FSharp.Data.WsdlProvider.DesignTime.dll")>]
do ()
