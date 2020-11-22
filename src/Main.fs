module rec Stuff

open FSharp.Data.Adaptive

[<AutoOpen>]
module Types =
    module Source =
        type IComponent =
            abstract member A: int
            abstract member B: int
            abstract member C: int
            abstract member Children: IComponent[]

    type Values =
        { A: aval<int>
          B: aval<int>
          C: aval<int> }

    type RootValues =
        { A: cval<int>
          B: cval<int>
          C: cval<int> }

    type AdaptiveHandlers =
        { A: aval<int>
          B: aval<int>
          C: aval<int> }

    type AdaptiveAccumulator = alist<int * int * int>

    type Component =
        { Values: Values
          Handlers: AdaptiveHandlers
          Children: Component[]
          Accumulator: AdaptiveAccumulator }

    type Root =
        { Values: RootValues
          Handlers: AdaptiveHandlers
          Children: Component[]
          Accumulator: AdaptiveAccumulator }

module Handlers =
    let noopHandler x = x |> AVal.map (fun x -> printfn "Noop handler: %d" x; x)
    let aHandler a = a |> AVal.map (fun a' -> printfn "A': %d" a'; a' + 1)
    let bHandler b = b |> AVal.map (fun b' -> printfn "B': %d" b'; b' - 1)
    let cHandler c = c |> AVal.map (fun c' -> printfn "C': %d" c'; c' * 2)

module FromSource =
    let private toRootValues a b c: RootValues =
        { A = AVal.init a
          B = AVal.init b
          C = AVal.init c }

    let private toComponentValues (parentValues: Values) a b c: Values =
        { A = parentValues.A |> AVal.map (fun pa -> pa + a)
          B = parentValues.B |> AVal.map (fun pb -> pb - b)
          C = parentValues.C |> AVal.map (fun pc -> pc * c) }

    let rec private ofComponent (parentValues: Values) (c: Source.IComponent): Component =
        let values = toComponentValues parentValues c.A c.B c.C
        let handlers: AdaptiveHandlers =
            { A = Handlers.aHandler values.A
              B = Handlers.bHandler values.B
              C = Handlers.cHandler values.C }
        let children = c.Children |> Array.map (ofComponent values)
        let accumulator =
            let childAccumulator = children |> AList.ofSeq |> AList.collect (fun c -> c.Accumulator)
            AVal.map3 (fun a b c -> a, b, c) handlers.A handlers.B handlers.C
            |> AList.single
            |> AList.mapA id
            |> AList.append childAccumulator
        { Values = values
          Handlers = handlers
          Children = children
          Accumulator = accumulator }

    let private ofRoot (c: Source.IComponent): Root =
        let values = toRootValues c.A c.B c.C
        let handlers: AdaptiveHandlers =
            { A = Handlers.noopHandler values.A
              B = Handlers.noopHandler values.B
              C = Handlers.noopHandler values.C }
        let asChildValues: Values =
            { A = values.A :> aval<_>
              B = values.B :> aval<_>
              C = values.C :> aval<_> }
        let children = c.Children |> Array.map (ofComponent asChildValues)
        let accumulator =
            let childAccumulator = children |> AList.ofSeq |> AList.collect (fun c -> c.Accumulator)
            AVal.map3 (fun a b c -> a, b, c) handlers.A handlers.B handlers.C
            |> AList.single
            |> AList.mapA id
            |> AList.append childAccumulator
        { Values = values
          Handlers = handlers
          Children = children
          Accumulator = accumulator }

    let create (c: Source.IComponent) = ofRoot c
