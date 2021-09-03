namespace SdlangFSharp

open FParsec
open System
open System.Collections.Generic;

module Parser =
    let ws = optional (skipMany (anyOf [' '; '\t']))
    let escapedNewLine = optional (skipChar '\\' >>. newline)
    let stringValue = 
        let unescaped = noneOf "\""
        let escaped = 
            pipe2 (skipChar '\\')  anyChar (
                fun _ ch -> match ch with
                            | 'n' -> '\n'
            )

        between (skipChar '"') (skipChar '"') (many (escaped <|> unescaped)) 
            |>> fun chlist -> SdlValue.String(System.String(Array.ofList(chlist)))

    let intValue = 
        opt (pchar '-') 
        .>>. many1Satisfy (fun ch -> Char.IsDigit ch)
        .>> optional (skipAnyOf [ 'l'; 'L' ])
        |>> fun a -> match a with
                     | (Some minus, number) -> Integer((int64 number) * -1L)
                     | (None, number) -> Integer(int64 number)

    let floatValue =
        opt (pchar '-')
        .>>. manySatisfy (fun ch -> Char.IsDigit ch)
        .>>  skipChar '.'
        .>>. manySatisfy (fun ch -> Char.IsDigit ch)
        .>> optional (skipAnyOf [ 'f'; 'F'; 'd'; 'D' ])
        |>> fun a -> match a with
                     | ((Some minus, number), fraction) -> Floating((double (number + "." + fraction)) * -1.);
                     | ((None, number), fraction) -> Floating(double (number + "." + fraction))

    let dateValue =
        manyMinMaxSatisfy 4 4 (fun ch -> Char.IsDigit ch)
        .>> skipChar '/'
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        .>> skipChar '/'
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        |>> fun a -> match a with
                     | ((y, m), d) -> Date(DateOnly(int y, int m, int d))

    let dateTimeValue =
        manyMinMaxSatisfy 4 4 (fun ch -> Char.IsDigit ch)
        .>> skipChar '/'
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        .>> skipChar '/'
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        .>> skipChar ' '
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        .>> skipChar ':'
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        .>> skipChar ':'
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        .>>. opt (
            skipChar '-'
            >>. manyMinMaxSatisfy 3 3 (fun ch -> Char.IsLetter ch)
            .>>. opt (
                (skipChar '-' <|> skipChar '+')
                >>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
            )
        )
        |>> fun a -> match a with
            | ((((((y, m), d), h), mi), s), tzoff) ->
                let ret = DateTimeOffset(int y, int m, int d, int h, int mi, int s, TimeSpan.Zero)
                match tzoff with
                | Some (tz, off) -> 
                    // TODO: Find timezone
                    match off with
                    | Some offset -> 
                        ret.Offset.Add(TimeSpan.FromHours(float offset)) |> ignore
                        ()
                    | None -> ()
                    ()
                | None -> ()
                ret
        |>> SdlValue.DateTime

    let timeSpanValue =
        opt (
            manySatisfy Char.IsDigit
            .>> skipString "d:"
        )
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        .>> skipChar ':'
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        .>> skipChar ':'
        .>>. manyMinMaxSatisfy 2 2 (fun ch -> Char.IsDigit ch)
        .>>. opt (
            skipChar '.'
            >>. manyMinMaxSatisfy 4 4 (fun ch -> Char.IsDigit ch)
        )
        |>> fun value -> match value with
                         | (((dh, m), s), ms) ->
                            let ts = TimeSpan.Zero
                            ts.Add(TimeSpan.FromMinutes(float m))
                              .Add(TimeSpan.FromSeconds(float s))
                              |> ignore
                            match dh with
                            | (Some days, hours) ->
                                ts.Add(TimeSpan.FromDays(float days))
                                  .Add(TimeSpan.FromHours(float hours))
                                |> ignore
                            | (None, hours) ->
                                ts.Add(TimeSpan.FromHours(float hours))
                                |> ignore
                            match ms with
                            | Some msecs ->
                                ts.Add(TimeSpan.FromMilliseconds(float msecs))
                                |> ignore
                            | None -> ()
                            ts
        |>> SdlValue.TimeSpan

    let boolValue = 
        (
            stringReturn "true" (SdlValue.Boolean true)
            <|> stringReturn "on" (SdlValue.Boolean true)
            <|> stringReturn "false" (SdlValue.Boolean false)
            <|> stringReturn "off" (SdlValue.Boolean false)
        ) .>> ((regex "[^a-zA-Z=]" |>> fun _ -> ()) <|> eof)
         
    let nullValue = stringReturn "null" (SdlValue.Null)

    let pvalue : Parser<SdlValue, unit> = choice [
        attempt boolValue
        nullValue
        stringValue
        attempt dateTimeValue
        attempt dateValue
        attempt timeSpanValue
        attempt floatValue
        intValue
    ]

    let psingleComment = 
        (skipString "//" <|> skipString "#" <|> skipString "--")
        >>. restOfLine true
        |>> Comment

    let pcomment = 
        spaces
        >>. psingleComment

    let pident =
        identifier (IdentifierOptions(isAsciiIdStart = Char.IsLetter, isAsciiIdContinue = Char.IsLetter))

    let pattribute =
        opt (attempt (pident .>> skipChar ':'))
        .>>. pident
        .>> skipChar '='
        .>>. pvalue
        |>> fun value -> match value with
                         | ((Some nspace, name), value) -> { 
                                Namespace = nspace
                                Name = name
                                Value = value
                             }
                         | ((None, name), value) -> {
                                Namespace = null
                                Name = name
                                Value = value
                             }

    let ptag =
        spaces
        >>. opt (attempt (pident .>> skipChar ':'))
        .>>. pident
        .>> ws
        .>>. many (escapedNewLine >>. ws >>. attempt pvalue .>> ws .>> escapedNewLine)
        .>>. many (escapedNewLine >>. ws >>. attempt pattribute .>> ws .>> escapedNewLine)
        .>> ws
        |>> fun value -> match value with
                         | (((Some nspace, name), values), attributes) -> (nspace, name, values, attributes)
                         | (((None, name), values), attributes) -> (null, name, values, attributes)
        |>> fun tuple -> match tuple with
                         | (nspace, name, values, attributes) ->
                            let tag = SdlTag(nspace, name)
                            tag.Values.AddRange(values)
                            tag.Attributes.AddRange(attributes)
                            tag

    let ptagChildrenStart = charReturn '{' ChildStart
    let ptagChildrenEnd   = spaces >>. charReturn '}' ChildEnd
    
    let psdl =
        many (attempt pcomment <|> attempt (ptag |>> Tag) <|> attempt ptagChildrenStart <|> attempt ptagChildrenEnd)

    let ParseSdlang (sdl: string) =
        let result = run psdl sdl
        let parentStack = Stack<SdlTag>()
        let mutable lastTag : SdlTag = SdlTag("<none>", "<root>")
        parentStack.Push lastTag

        match result with
        | Success (s,_,_) -> 
            for ast in s do
                match ast with
                | Tag t -> 
                    parentStack.Peek().Children.Add t
                    lastTag <- t
                | ChildStart -> parentStack.Push(lastTag)
                | ChildEnd ->
                    if parentStack.Count = 1 then
                        failwith "Unmatched '}', attempted to pop the root node"
                    parentStack.Pop() |> ignore
                | Comment c -> ()
                | v -> failwith $"Unexpected node: {v}"
        | Failure (msg,_,_) -> failwith msg
        if parentStack.Count <> 1 then
            failwith "Unmatched '{', there are still non-root nodes on the parent stack"
        parentStack.Peek()