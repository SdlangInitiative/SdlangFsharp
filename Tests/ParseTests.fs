module ParseTests

open System
open Xunit
open SdlangFSharp.Parser
open SdlangFSharp
open FParsec
open Xunit.Sdk

let parse p str =
    match run p str with
    | Success (res, _, _) -> res
    | Failure (msg, _, _) -> 
        Assert.False(true, msg) 
        raise (Exception(""))

[<Fact>]
let ``string value`` () =
    let result = parse pvalue "\"abc\none\\ntwo\""
    match result with
    | String s -> Assert.Equal("abc\none\ntwo", s);
    | _ -> raise (XunitException("Did not recieve a string"))

[<Fact>]
let ``int value`` () =
    let result = parse pvalue "-1230L"
    match result with
    | Integer i -> Assert.Equal(-1230L, i)
    | _ -> raise (XunitException("Did not recieve an int"))

[<Fact>]
let ``float value`` () =
    let result = parse pvalue "-1230.1234F"
    match result with
    | Floating f -> Assert.Equal(-1230.1234, f)
    | _ -> raise (XunitException("Did not recieve a float"))

[<Fact>]
let ``date value`` () =
    let result = parse pvalue "1111/11/11"
    match result with
    | Date d -> Assert.Equal(DateOnly(1111, 11, 11), d)
    | _ -> raise (XunitException("Did not recieve a date"))

[<Fact>]
let ``datetime value`` () =
    let result = parse pvalue "1111/11/11 11:11:11-GMT+00"
    match result with
    | DateTime d -> Assert.Equal(DateTimeOffset(1111, 11, 11, 11, 11, 11, TimeSpan.Zero), d)
    | _ -> raise (XunitException("Did not recieve a datetime"))

[<Fact>]
let ``timespan value`` () =
    let result = parse pvalue "11d:00:00:00.0000"
    match result with
    | TimeSpan _ -> ()
    | _ -> raise (XunitException("Did not recieve a timespan"))

[<Fact>]
let ``boolean value`` () =
    let result = parse pvalue "true"
    match result with
    | Boolean b -> Assert.True(b)
    | _ -> raise (XunitException("Did not recieve a bool"))

[<Fact>]
let ``attribute`` () =
    let result = parse pattribute "test=1111/11/11 11:11:11"
    match result.Value with
    | DateTime _ -> ()
    | _ -> raise (XunitException("Did not recieve a bool"))

[<Fact>]
let ``tag`` () =
    let result = (ParseSdlang "a:b \"c\" one=23").Children.[0]
    Assert.Equal(1, result.Values.Count)
    Assert.Equal(1, result.Attributes.Count)
    Assert.Equal(Some "c", result.GetValueAsString(0))
    Assert.Equal(Some 23L, result.GetAttribute("one").Value.AsInteger())
    ()