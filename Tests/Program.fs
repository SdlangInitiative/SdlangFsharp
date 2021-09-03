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
        0
