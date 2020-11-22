module Tests

// Adapted from: https://github.com/MangelMaxime/Thoth/blob/153c7f9484d3ff53b70b1fb232a0b14936221331/tests/Main.fs
// MIT licensed: https://github.com/MangelMaxime/Thoth/blob/153c7f9484d3ff53b70b1fb232a0b14936221331/LICENSE.md
open FSharp.Data.Adaptive
open Stuff
open Util

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

let run () =
    let adpativeTests =
        testList "First tests"
            [ testList "Inner tests" [
                testCase "Adaptive test" <| fun () ->
                    let root = Stuff.FromSource.create SourceData.root
                    let child = root.Children.[0]
                    root.Accumulator |> AList.force |> ignore

                    transact (fun () -> root.Values.A.Value <- 0)
                    root.Accumulator |> AList.force |> ignore

                    equal 0 0 ] ]

    let tests =
        [ adpativeTests ] :> Test seq

    runTests tests

run ()
