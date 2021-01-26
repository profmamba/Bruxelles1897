module Domain

open Deck

type PlayerCount = 
  | TwoPlayers
  | ThreePlayers
  | FourPlayers

type PlayerColor =
  | YellowPlayer
  | GreenPlayer
  | BluePlayer
  | RedPlayer

type ArchitectType =
  | OneAndThree
  | TwoAndFour
  | TwoAndFive

type Architect = {
  Type: ArchitectType
  Value: int
  Color: PlayerColor
}

type Artwork = 
  | YellowArt
  | GreenArt
  | BlueArt
  | RedArt

// The houses are most easily distinguished by the different lamp posts on them
type House = 
  | SolidSingleLamp
  | SolidDoubleLamp
  | SingleLamp
  | DoubleLamp

type Noble =
  | Brugmann
  | Horta
  | Hankar
  | Vandervelde
  | Albert
  | Buls

type BEF = BEF

type Bonus =
  | Nobility
  | Architecture
  | Prestige
  | Prison

type SingleMaterial = 
  | Wood
  | Brick
  | Metal

type Material = SingleMaterial * SingleMaterial

type BruxellesAction =
  | StockExchange 
  | CityHallBEF | CinquantenairePark

type BruxellesArea = {
  StockExchange: Architect list
  CityHall: Architect list
  CinquantenairePark: Architect list
}

type ArtNoveauAction =
  | Creation of Artwork
  | Sale of BEF
  | Supply of Material
  | Construction of House
  | Influence of Noble
  | Exhibition

type ArtNoveauAreaCell = 
  | Action of ArtNoveauAction
  | Architect of Architect

type ArtNoveauColumn =  { 
  Actions: ArtNoveauAreaCell list
  Bonus: Bonus
}

type ArtNoveauArea = 
  ArtNoveauColumn list



type ArtworkDeck = DeckWithDiscard<Artwork>

type MaterialDeck = DeckWithDiscard<Material>

type HouseDeck = DeckWithDiscard<House>

type NobleDeck = DeckWithDiscard<Noble>

type PlayerNoble = {
  Noble: Noble
  Used: bool
}

type Player = {
  Color: PlayerColor
  Nobles: PlayerNoble list
  Houses: House list
  Artworks: Artwork list
  Materials: Material list
  Architects: Architect list
  BEF: int
  Score: int
  Noblity: int
  Architecture: int
  Prestige: int
  PrestigeLevel: int
  Passed: bool
}

type GameState = {
  ArtNoveauArea: ArtNoveauArea
  BruxellesArea: BruxellesArea
  ArtworkDeck: ArtworkDeck
  MaterialDeck: MaterialDeck
  NobleDeck: NobleDeck
  HouseDeck: HouseDeck 
  Prison: Architect list
  Round: int
  PlayerCount: PlayerCount
  Players: Player list
  PlayerOrder: PlayerColor list
  CurrentPlayer: PlayerColor
  ExhibitionPlayer: PlayerColor option
}

