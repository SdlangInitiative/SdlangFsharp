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
with
    member this.QualifiedName() = if this.Namespace <> null then this.Namespace + ":" + this.Name else this.Name 
    member this.AsString()      = match this.Value with String v    -> Some v | _ -> None
    member this.AsInteger()     = match this.Value with Integer v   -> Some v | _ -> None
    member this.AsFloating()    = match this.Value with Floating v  -> Some v | _ -> None
    member this.AsDate()        = match this.Value with Date v      -> Some v | _ -> None
    member this.AsDateTime()    = match this.Value with DateTime v  -> Some v | _ -> None
    member this.AsTimeSpan()    = match this.Value with TimeSpan v  -> Some v | _ -> None
    member this.AsBoolean()     = match this.Value with Boolean v   -> Some v | _ -> None
    member this.IsNull()        = match this.Value with Null        -> true   | _ -> false

type SdlTag(nspace: string, name: string) =
    member val Namespace    = nspace with get, set
    member val Name         = name with get, set
    member val Values       = new List<SdlValue>() with get, set
    member val Attributes   = new List<SdlAttribute>() with get, set
    member val Children     = new List<SdlTag>() with get, set

    member this.QualifiedName() = if this.Namespace <> null then this.Namespace + ":" + this.Name else this.Name 
    member this.GetAttribute(fqn) = 
        this.Attributes 
        |> Seq.filter (fun a -> a.QualifiedName() = fqn)
        |> Seq.tryExactlyOne
    member this.GetValue(index)                 = if index >= this.Values.Count then None else Some this.Values.[index]
    member this.GetChild(index)                 = if index >= this.Children.Count then None else Some this.Children.[index]
    member this.GetChildrenBy(pred)             = this.Children |> Seq.filter pred
    member this.GetChildrenByName(name)         = this.GetChildrenBy (fun c -> c.Name = name)
    member this.GetChildrenByNamespace(name)    = this.GetChildrenBy (fun c -> c.Namespace = name)
    member this.GetChildrenByFQN(name)          = this.GetChildrenBy (fun c -> c.QualifiedName() = name)
    member this.GetValueAsString(index)         = match this.Values.[index] with String v    -> Some v | _ -> None
    member this.GetValueAsInteger(index)        = match this.Values.[index] with Integer v   -> Some v | _ -> None
    member this.GetValueAsFloating(index)       = match this.Values.[index] with Floating v  -> Some v | _ -> None
    member this.GetValueAsDate(index)           = match this.Values.[index] with Date v      -> Some v | _ -> None
    member this.GetValueAsDateTime(index)       = match this.Values.[index] with DateTime v  -> Some v | _ -> None
    member this.GetValueAsTimeSpan(index)       = match this.Values.[index] with TimeSpan v  -> Some v | _ -> None
    member this.GetValueAsBoolean(index)        = match this.Values.[index] with Boolean v   -> Some v | _ -> None
    member this.GetValueIsNull(index)           = match this.Values.[index] with Null        -> true   | _ -> false

type SdlAst = 
    | Value of SdlValue
    | Attribute of SdlAttribute
    | Tag of SdlTag
    | ChildStart
    | ChildEnd
    | Comment of string