[<AutoOpen>]
module FSharp.Data.Prelude
open System

module String =
    let camlCase (s: string) =
        if s.Length >= 1 then
            string (Char.ToLowerInvariant(s[0])) + s.Substring(1);
        else
            s
    let PascalCase (s: string) =
        if s.Length >= 1 then
            string (Char.ToUpperInvariant(s[0])) + s.Substring(1);
        else
            s

let inline typenameof<'t> = typeof<'t>.FullName


