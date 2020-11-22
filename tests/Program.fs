module Program

[<EntryPoint>]
let main argv =
    let root = Tests.TestData.buildAndUpdate ()
    0