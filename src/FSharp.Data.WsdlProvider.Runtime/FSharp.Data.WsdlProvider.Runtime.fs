namespace FSharp.Data

open System
open System.Text
open System.ServiceModel
open System.ServiceModel.Channels

type DefaultBinding =
    static member SelectBinding(uri: string) : Binding =
        DefaultBinding.SelectBinding(uri, false)

    static member SelectBinding(uri: string, soap12: bool) : Binding =

        let isHttps =
            (Uri uri).Scheme = Uri.UriSchemeHttps

        if soap12 then

            let encoding =
                TextMessageEncodingBindingElement(
                    MessageVersion.Soap12,
                    Encoding.UTF8
                )

            let transport : BindingElement =
                if isHttps then
                    HttpsTransportBindingElement() :> BindingElement
                else
                    HttpTransportBindingElement() :> BindingElement

            CustomBinding(encoding, transport) :> Binding

        elif isHttps then
            BasicHttpsBinding() :> Binding

        else
            BasicHttpBinding() :> Binding


#if !DESIGNTIME
// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("FSharp.Data.WsdlProvider.DesignTime.dll")>]
do ()
#endif
