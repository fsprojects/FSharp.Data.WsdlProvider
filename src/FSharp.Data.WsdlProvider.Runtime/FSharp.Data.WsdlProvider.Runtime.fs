namespace MyNamespace

open System

// Put any runtime constructs here
type DataSource(filename:string) = 
    member this.FileName = filename


// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("FSharp.Data.WsdlProvider.DesignTime.dll")>]
do ()
