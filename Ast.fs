namespace SdlangFSharp
open System
open System.Collections.Generic

type SdlValue =
    | String of string
    | Integer of int64
    | Floating of double
    | Date of DateOnly
    | DateTime of DateTimeOffset
    | TimeSpan of TimeSpan
    | Boolean of bool
    | Null

type SdlAttribute = {
    Namespace: string
    Name: string
    Value: SdlValue
}

type SdlTag(nspace: string, name: string) =
    member val Namespace = nspace with get, set
    member val Name = name with get, set
    member val Values = (new List<SdlValue>()) with get, set
    member val Attributes = new List<SdlAttribute>() with get, set
    member val Children = new List<SdlTag>() with get, set

    member this.QualifiedName = this.Namespace + ":" + this.Name

type SdlAst = 
    | Value of SdlValue
    | Attribute of SdlAttribute
    | Tag of SdlTag
    | ChildStart
    | ChildEnd
    | Comment of string