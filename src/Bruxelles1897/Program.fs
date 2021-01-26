// Learn more about F# at http://fsharp.org

open Domain
open Setup

[<EntryPoint>]
let main argv =
    let gameState = 
      createGameState TwoPlayers
      |> setupNextRound (printfn "%A")
    
    0 // return an integer exit code
