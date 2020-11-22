module Program

let a = 3

#if !FABLE_COMPILER
[<EntryPoint>]
let main argv =
    let first, second = Tests.TestData.simple ()
    printfn "First: %d" first.[0]
    printfn "Second: %d" second.[0]
    0
#endif