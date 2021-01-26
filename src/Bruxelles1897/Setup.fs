module Setup

open FSharpPlus
open Deck
open Domain

module Instrumentation =
  type SetupInstrumentationEvent = 
  | GameStateUpdatedEvent of GameState
  | ArtNoveauDeckCreated of ArtNoveauAction list

let seedArtworkDeck = 
  List.empty
  |> addToDeck YellowArt 3
  |> addToDeck BlueArt 3
  |> addToDeck GreenArt 3
  |> addToDeck RedArt 3

let seedMaterialDeck =
  List.empty
  |> addToDeck (Material(Wood, Wood)) 2
  |> addToDeck (Material(Brick, Wood)) 2
  |> addToDeck (Material(Metal, Metal)) 2
  |> addToDeck (Material(Metal, Wood)) 2
  |> addToDeck (Material(Metal, Brick)) 2
  |> addToDeck (Material(Brick, Brick)) 2

let seedNobleDeck =   
  List.empty
  |> addToDeck Horta 3
  |> addToDeck Buls 3
  |> addToDeck Hankar 3
  |> addToDeck Vandervelde 3
  |> addToDeck Albert 3

let seedHouseDeck =
  List.empty
  |> addToDeck SolidSingleLamp 4
  |> addToDeck SolidDoubleLamp 4
  |> addToDeck SingleLamp 4
  |> addToDeck DoubleLamp 4

let seedBonusDeck =
  List.empty
  |> addToDeck Nobility 1
  |> addToDeck Architecture 1
  |> addToDeck Prestige 1
  |> addToDeck Prison 1

let createPlayerArchitects playerColor playerCount =
  let architects = 
    List.init 3 (fun _ -> { Type = OneAndThree; Value = 1; Color = playerColor})
    |> List.append [
      { Type = OneAndThree; Value = 3; Color = playerColor} 
      { Type = TwoAndFour; Value = 2; Color = playerColor} ]
  match playerCount with
  | TwoPlayers -> { Type = TwoAndFour; Value = 2; Color = playerColor} :: architects
  | _ -> architects

let createPrisonArchitects playerColors =
  let a t = playerColors |>> (fun c -> { Type = t; Value = 2; Color = c} )
  let prison = a TwoAndFive
  if List.length playerColors > 2 
    then List.append prison (a TwoAndFour) 
    else prison

let createPlayer playerColor startingNoble bonusBEF architects = {
  Color = playerColor
  Nobles = [ { Noble = startingNoble; Used = false } ] 
  Houses = List.empty
  Artworks = List.empty
  Materials = List.empty
  Architects = architects
  BEF = 
    match startingNoble with
    | Brugmann ->  4 + bonusBEF
    | _ -> 0
  Score = 0
  Noblity = 1
  Architecture = 
    match startingNoble with
    | Horta -> 2
    | _ -> 1
  Prestige = 1
  PrestigeLevel = 1
  Passed = false
}

let createPlayers = function
  | TwoPlayers -> [ 
    (createPlayer GreenPlayer Horta 0 (createPlayerArchitects GreenPlayer TwoPlayers)); 
    (createPlayer RedPlayer Buls 0 (createPlayerArchitects RedPlayer TwoPlayers))]
  | _ -> failwith "only two player bot game available"

let createGameState playerCount = {
  ArtNoveauArea = List.empty
  BruxellesArea = {
    StockExchange = List.empty
    CityHall = List.empty
    CinquantenairePark = List.empty
  }
  ArtworkDeck = {
    Deck = seedArtworkDeck
    Discard = List.empty
  }
  MaterialDeck = {
    Deck = seedMaterialDeck
    Discard = List.empty
  }
  NobleDeck = {
    Deck = seedNobleDeck
    Discard = List.empty
  }
  HouseDeck = {
    Deck = seedHouseDeck
    Discard = List.empty
  }
  Prison = createPrisonArchitects [GreenPlayer; RedPlayer] //todo add initial architects to prison
  PlayerCount = playerCount
  Players = createPlayers playerCount
  PlayerOrder = List.Empty 
  CurrentPlayer = BluePlayer
  Round = 0
  ExhibitionPlayer = None
}

let addCardsToNoveauDeck (cards: 'a list) mapper mainDeck = 
  cards |>> mapper |> List.append mainDeck

let rec createNoveauColumns deck bonusDeck (rowCounts : int[]) index state  =
  match index with
  | 4 -> state, deck
  | _ -> 
    let (columnActions, newDeck) = drawRandomCardsFromDeck deck rowCounts.[index]
    let (bonus, newBonusDeck) = drawRandomCardsFromDeck bonusDeck 1

    createNoveauColumns 
      newDeck 
      newBonusDeck
      rowCounts
      (index + 1) 
      ({ Actions = columnActions |>> Action; Bonus = bonus.Head } :: state) 

let setupNextRound 
   : InstrumentationSink<Instrumentation.SetupInstrumentationEvent>
  -> GameState
  -> GameState = 
  fun logSetupEvent gameState ->
    
    // prepare cards for noveau deck
    let (drawnArtworks, artworkDeck) = drawRandomCardsFromDeckWithReplenish gameState.ArtworkDeck 3
    let (drawnMaterials, materialDeck) = drawRandomCardsFromDeckWithReplenish gameState.MaterialDeck 3
    let (drawnNobles, nobleDeck) = drawRandomCardsFromDeckWithReplenish gameState.NobleDeck 3
    let (drawnHouses, houseDeck) = drawRandomCardsFromDeckWithReplenish gameState.HouseDeck 3

    printfn "%A" drawnArtworks
    printfn "%A" artworkDeck

    // create the noveau deck
    let noveauDeck = 
      List.empty
      |> addCardsToNoveauDeck drawnArtworks (fun i -> Creation i)
      |> addCardsToNoveauDeck drawnMaterials (fun i -> Supply i)
      |> addCardsToNoveauDeck drawnNobles (fun i -> Influence i)
      |> addCardsToNoveauDeck drawnHouses (fun i -> Construction i)
      |> addToDeck (Sale BEF) 3
      |> addToDeck (Exhibition) 1  
    
    logSetupEvent <| Instrumentation.ArtNoveauDeckCreated noveauDeck

    // determine the number of cards per column based on number of players
    let rowCounts = 
      match gameState.PlayerCount with
      | TwoPlayers -> [| 2; 3; 3; 2 |]
      | ThreePlayers -> [| 3; 3; 3; 3 |]
      | FourPlayers -> [| 4; 4; 4; 4 |]
  
    // populate the noveau area from the deck and keep track of unused cards
    let artNoveauArea, unusedCards = createNoveauColumns noveauDeck seedBonusDeck rowCounts 0 List.empty 

    // algud, create new game state
    let newGameState = 
      { 
        gameState with
          Round = gameState.Round + 1
          ExhibitionPlayer = None
          ArtNoveauArea = artNoveauArea
          ArtworkDeck = artworkDeck
          MaterialDeck = materialDeck
          HouseDeck = houseDeck
          NobleDeck = nobleDeck
      }

    // return unused cards from noveau deck to respective discards
    let newGameStateWithDiscard = 
      List.fold (
        fun s -> function
        | Creation x -> { s with ArtworkDeck = discardCard s.ArtworkDeck x}
        | Supply x -> { s with MaterialDeck = discardCard s.MaterialDeck x}
        | Construction x -> { s with HouseDeck = discardCard s.HouseDeck x}
        | Influence x -> { s with NobleDeck = discardCard s.NobleDeck x}
        | _ -> s) newGameState unusedCards

    logSetupEvent <| Instrumentation.GameStateUpdatedEvent newGameStateWithDiscard

    newGameStateWithDiscard