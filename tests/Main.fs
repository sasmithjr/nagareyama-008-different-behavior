module Tests

// Adapted from: https://github.com/MangelMaxime/Thoth/blob/153c7f9484d3ff53b70b1fb232a0b14936221331/tests/Main.fs
// MIT licensed: https://github.com/MangelMaxime/Thoth/blob/153c7f9484d3ff53b70b1fb232a0b14936221331/LICENSE.md
open FSharp.Data.Adaptive
#if FABLE_COMPILER
open Util
#endif

module TestData =
    let simple () =
        let x = AVal.init 3
        let mappedX = x |> AVal.map (fun x' -> printfn "X: %d" x'; x' + 1)
        let adaptiveList =
            mappedX
            |> AList.single
            |> AList.mapA id
        let firstForce = adaptiveList |> AList.force
        transact (fun () -> x.Value <- 0)
        let secondForce = adaptiveList |> AList.force
        firstForce, secondForce

#if FABLE_COMPILER
let run () =
    let adpativeTests =
        testList "First tests"
            [ testList "Inner tests" [
                testCase "Adaptive test" <| fun () ->
                    let first, second = TestData.simple ()

                    equal 4 first.[0]
                    equal 1 second.[0] ] ]

    let tests =
        [ adpativeTests ] :> Test seq

    runTests tests

run ()
#endif