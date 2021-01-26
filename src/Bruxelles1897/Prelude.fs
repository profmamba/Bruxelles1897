[<AutoOpen>]
module internal Prelude

let genRandomNumbers upperLimit count=
  let rnd = System.Random()
  let initial = Seq.initInfinite (fun _ -> rnd.Next (0, upperLimit)) 
  initial
  |> Seq.distinct
  |> Seq.take(count)
  |> Seq.toList

let pick lst indexes= 
  [for i in indexes do yield (List.item i lst)]

type InstrumentationSink<'a> = 'a -> unit

