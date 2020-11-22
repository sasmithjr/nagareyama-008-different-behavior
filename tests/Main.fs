module Tests

// Adapted from: https://github.com/MangelMaxime/Thoth/blob/153c7f9484d3ff53b70b1fb232a0b14936221331/tests/Main.fs
// MIT licensed: https://github.com/MangelMaxime/Thoth/blob/153c7f9484d3ff53b70b1fb232a0b14936221331/LICENSE.md
open FSharp.Data.Adaptive
open Stuff
#if FABLE_COMPILER
open Util
#endif

module SourceData =
    open Stuff.Types.Source

    let child: IComponent =
        { new IComponent with
            member _.A = 1
            member _.B = 2
            member _.C = 3
            member _.Children = Array.empty }

    let root: IComponent =
        { new IComponent with
            member _.A = 4
            member _.B = 5
            member _.C = 6
            member _.Children = [| child |] }

module TestData =
    let buildAndUpdate () =
        let root = Stuff.FromSource.create SourceData.root
        root.Accumulator |> AList.force |> ignore

        transact (fun () -> root.Values.A.Value <- 0)
        let _accumulated = root.Accumulator |> AList.force

        root

#if FABLE_COMPILER
let run () =
    let adpativeTests =
        testList "First tests"
            [ testList "Inner tests" [
                testCase "Adaptive test" <| fun () ->
                    let root = TestData.buildAndUpdate ()
                    equal 0 0 ] ]

    let tests =
        [ adpativeTests ] :> Test seq

    runTests tests

run ()
#endif