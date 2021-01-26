module Deck
  
type DeckWithDiscard<'a> = {
  Deck: 'a list
  Discard: 'a list
}

let addToDeck item count deck = 
  List.append deck [for i in [1..count] do yield item]

let discardFromDeck deckWithDiscard card  =
  {deckWithDiscard with Discard = card :: deckWithDiscard.Discard}

let drawRandomCardsFromDeck deck count = 
  // pick some random indexes in the deck to draw
  let drawIndexes = genRandomNumbers (List.length deck) count

  // remaining = everything except drawn cards
  let remainingIndexes = [0..((List.length deck) - 1)] |> List.except drawIndexes

  let drawCards = drawIndexes |> pick deck
  let remainingCards = remainingIndexes |> pick deck
  
  (drawCards, remainingCards)