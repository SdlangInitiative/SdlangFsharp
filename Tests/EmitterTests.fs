module EmitterTests

open System
open SdlangFSharp
open Xunit

let emit root = Emitter.EmitSdlang root

[<Fact>]
let ``string`` () =
    let tag = SdlTag(null, "tag")
    tag.Values.Add (SdlValue.String("Henlo, world!"))
    let sdl = emit tag
    Assert.Equal ("\"Henlo, world!\" ", sdl)
    ()

[<Fact>]
let ``int`` () =
    let tag = SdlTag(null, "tag")
    tag.Values.Add (SdlValue.Integer(420L))
    let sdl = emit tag
    Assert.Equal ("420 ", sdl)
    ()

[<Fact>]
let ``float`` () =
    let tag = SdlTag(null, "tag")
    tag.Values.Add (SdlValue.Floating(420.69))
    let sdl = emit tag
    Assert.Equal ("420.69 ", sdl)
    ()

[<Fact>]
let ``Date`` () =
    let tag = SdlTag(null, "tag")
    tag.Values.Add (SdlValue.Date(DateOnly(2021, 09, 03)))
    let sdl = emit tag
    Assert.Equal ("2021/09/03 ", sdl)
    ()

[<Fact>]
let ``DateTime`` () =
    let tag = SdlTag(null, "tag")
    tag.Values.Add (SdlValue.DateTime(DateTimeOffset(2021, 09, 03, 22, 54, 10, TimeSpan.FromHours 1.)))
    let sdl = emit tag
    Assert.Equal ("2021/09/03 22:54:10.0000-GMT+1 ", sdl)
    ()
    
[<Fact>]
let ``Children`` () =
    let root    = SdlTag(null, null)
    let pet     = SdlTag("pet", "dog")
    let owner   = SdlTag(null, "owner")

    root.Children.Add pet
    pet.Children.Add  owner

    pet.Values.Add (SdlValue.String("Cooper"))
    pet.Attributes.Add ({ Namespace = "is"; Name = "cute"; Value = SdlValue.Boolean(true) })

    owner.Values.Add (SdlValue.String("Bradley"))

    let sdl = Emitter.EmitSdlang (root)
    Assert.Equal("pet:dog \"Cooper\" is:cute=true {\n    owner \"Bradley\" \n}\n", sdl)
    ()