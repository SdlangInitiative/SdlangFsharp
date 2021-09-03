# Overview

This is an `F#` library for parsing and emitting [SDLang](https://sdlang.org/) files.

# Installation

yada yada todo

# Usage for parsing

* Call `SdlangFSharp.Parser.ParseSdlang` on a string containing a valid SDLang file.

```fsharp
module Program
    open SdlangFSharp

    let [<EntryPoint>] main _ =
        let code = """pet:dog "Cooper" is:cute=true {
            owner "Bradley"
        }
        """
        let root = Parser.ParseSdlang (code)
        let pet = Seq.head (root.GetChildrenByNamespace("pet"))
        assert (pet.QualifiedName = "pet:dog")
        assert (pet.Namespace = "pet")
        assert (pet.Name = "dog")
        assert (pet.GetValueAsString(0).Value = "Cooper")
        assert (pet.GetAttribute("is:cute").Value.AsBoolean().Value)
        let owner = Seq.head (pet.GetChildrenByName("owner"))
        assert (owner.Name = "owner")
        assert (owner.GetValueAsString(0).Value = "Bradley")
        0
```

# Usage for building AST and emitting

```fsharp
module Program
    open SdlangFSharp

    let [<EntryPoint>] main _ =
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
        0
```