#r "paket:
source https://api.nuget.org/v3/index.json
framework: netstandard2.0
nuget FSharp.Core < 5
nuget Fake.Core 
nuget Fake.Core.Target
nuget Fake.Core.ReleaseNotes
nuget Fake.DotNet.Cli 
nuget Fake.DotNet.Paket //"

#load ".fake/build.fsx/intellisense.fsx"
open System
open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.DotNet

module BuildPath =
    let root = __SOURCE_DIRECTORY__
    let bin = root </> "bin"
    let nuget = bin </> "nuget"
    let test = bin </> "test"


    let sln = Directory.findFirstMatchingFile "*.sln" root
    let releaseNotes = root </> "RELEASE_NOTES.md"


let releaseNotes = ReleaseNotes.load BuildPath.releaseNotes

let version =
    let semver = releaseNotes.SemVer
    let build = 
        Environment.environVarOrNone "GITHUB_RUN_NUMBER"
        |> Option.bind (fun v ->
            match UInt32.TryParse v with
            | true, n -> Some n
            | false, _ -> None) 
        |> Option.defaultValue 0u

    { semver with Original = None
                  Patch = build }
    


Target.create "Clean" <| fun _ ->
    Directory.delete BuildPath.bin


Target.create "Build" <| fun _ ->
    DotNet.build (fun p ->
        { p with 
            Configuration = DotNet.BuildConfiguration.Release } )
        BuildPath.sln


Target.create "Test" <| fun _ ->
    let fx =
        if Environment.isWindows then
            None
        else
            Some "netcoreapp3.1"
    
    DotNet.test (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            NoBuild = true
            Framework = fx
            ResultsDirectory  = Some BuildPath.test
            } )
        BuildPath.sln


Target.create "Nuget" <| fun _ ->
    Paket.pack (fun p ->
        { p with 
            BuildConfig = "Release"
            OutputPath = BuildPath.nuget
            Version = version.AsString
            ToolType = ToolType.CreateLocalTool() }
    )
Target.create "All" ignore

"Clean" 
    ==> "Build"
    ==> "Test"
    ==> "Nuget"
    ==> "All"

Target.runOrDefault "All"





