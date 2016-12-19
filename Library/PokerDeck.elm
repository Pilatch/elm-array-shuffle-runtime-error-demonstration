module Library.PokerDeck exposing (add, PokerDeck, full, toList)

import Library.PokerSuit as PokerSuit exposing (PokerSuit(..))
import Library.PokerCard as PokerCard exposing (..)
import Library.CardCollection as CardCollection
import List


type alias PokerDeck =
    CardCollection.CardCollection


full : PokerDeck
full =
    let
        numberedRanks =
            List.range 2 14 |> List.map PokerCard.Number
    in
        new_
            [ Spades, Hearts, Clubs, Diamonds ]
            numberedRanks
            CardCollection.empty


add : PokerDeck -> PokerCard -> PokerDeck
add deck card =
    case card of
        RankedPokerSuited { rank, suit, facing } ->
            PokerCardAttributes rank suit Down |> RankedPokerSuited |> CardCollection.add deck


toList : PokerDeck -> List PokerCard
toList deck =
    CardCollection.toList deck



--- PRIVATE ---


new_ : List PokerSuit -> List PokerCard.Rank -> PokerDeck -> PokerDeck
new_ suits ranks output =
    case suits of
        [] ->
            output

        suit :: otherPokerSuits ->
            suitCards_ suit ranks
                |> CardCollection.append output
                |> new_ otherPokerSuits ranks


suitCards_ : PokerSuit -> List PokerCard.Rank -> PokerDeck
suitCards_ suit ranks =
    List.map (\rank -> PokerCardAttributes rank suit Down |> PokerCard.RankedPokerSuited) ranks |> CardCollection.fromList
