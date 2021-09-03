namespace SdlangFSharp

open System.Text

module Emitter =
    let private (<<.) (builder: StringBuilder) (str: string) = builder.Append str
    let private (<<!) number (count: int) = number.ToString().PadLeft(count, '0')

    let private EmitValue(value: SdlValue) = 
        match value with
        | String s      -> $"\"{s}\""
        | Integer i     -> $"{i}"
        | Floating f    -> $"{f}"
        | Date d        -> $"{d.Year <<! 4}/{d.Month <<! 2}/{d.Day <<! 2}"
        | TimeSpan t    -> $"{t.Days}d:{t.Hours <<! 2}:{t.Minutes <<! 2}:{t.Seconds <<! 2}.{t.Milliseconds <<! 4}"
        | Boolean b     -> $"{b}".ToLower()
        | Null          -> "null"
        | DateTime dt   ->
            let tz = dt.Offset.TotalHours
            let tzstr = (if tz < 0. then "GMT-" else "GMT+") + $"{tz}"
            $"{dt.Year <<! 4}/{dt.Month <<! 2}/{dt.Day <<! 2} {dt.Hour <<! 2}:{dt.Minute <<! 2}:{dt.Second <<! 2}.{dt.Millisecond <<! 4}-{tzstr}"

    let rec private EmitTag(builder: StringBuilder, tag: SdlTag, isRoot: bool, level: int) =
        let indent = if not isRoot then (Seq.replicate (level*4) " " |> Seq.fold (fun a b -> a + b) "") else ""

        if not isRoot then
            builder 
            <<. indent
            <<. tag.QualifiedName()
            <<. " "
            |> ignore

        for value in tag.Values do
            builder <<. EmitValue value <<. " " |> ignore
        for attrib in tag.Attributes do
            builder 
            <<. attrib.QualifiedName()
            <<. "="
            <<. EmitValue attrib.Value
            <<. " "
            |> ignore

        if tag.Children.Count <> 0 then
            if not isRoot then builder <<. "{\n" |> ignore
            for child in tag.Children do
                EmitTag (builder, child, false, level + 1)

            if not isRoot then
                builder 
                <<. indent
                <<. "}" 
                |> ignore
        if not isRoot then builder <<. "\n" |> ignore
        ()

    let EmitSdlang(root: SdlTag) =
        let builder = StringBuilder()
        EmitTag (builder, root, true, -1)
        builder.ToString()