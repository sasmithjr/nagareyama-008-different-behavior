module Util

// https://github.com/MangelMaxime/Thoth/blob/153c7f9484d3ff53b70b1fb232a0b14936221331/tests/Util.fs
// MIT licensed: https://github.com/MangelMaxime/Thoth/blob/153c7f9484d3ff53b70b1fb232a0b14936221331/LICENSE.md
open Fable.Core
open Fable.Core.Testing

[<Global>]
let describe (name: string) (f: unit -> unit) = jsNative

[<Global>]
let it (msg: string) (f: unit -> unit) = jsNative

type TestCase = string * (unit -> unit)

let testList (name: string) (tests: seq<'b>) = name, tests
let testCase (msg: string) (test: unit -> unit) = msg, test

let equal expected actual: unit = Assert.AreEqual(expected, actual)

type Test = string * seq<string * seq<TestCase>>

let private runTest ((msg, test): TestCase) = it msg test

let runTests tests =
    for (moduleName, moduleTests) in tests do
        describe moduleName
        <| fun () ->
            for (name, tests) in moduleTests do
                describe name
                <| fun _ ->
                    for test in tests do
                        runTest test
