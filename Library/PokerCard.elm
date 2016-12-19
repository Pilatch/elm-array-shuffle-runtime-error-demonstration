module Library.PokerCard exposing (..)

import Library.PokerSuit as PokerSuit exposing (PokerSuit(..))


type Facing
    = Up
    | Down


type Rank
    = Number Int
    | Joker


type PokerCard
    = RankedPokerSuited PokerCardAttributes


type alias PokerCardAttributes =
    { rank : Rank, suit : PokerSuit, facing : Facing }


flip : PokerCard -> PokerCard
flip card =
    case card of
        RankedPokerSuited { rank, suit, facing } ->
            case facing of
                Up ->
                    PokerCardAttributes rank suit Down |> RankedPokerSuited

                Down ->
                    PokerCardAttributes rank suit Up |> RankedPokerSuited


rankToString : Rank -> String
rankToString rank =
    case rank of
        Number int ->
            if int == 11 then
                "Jack"
            else if int == 12 then
                "Queen"
            else if int == 13 then
                "King"
            else if int == 14 then
                "Ace"
            else
                Basics.toString int

        Joker ->
            "Joker"


toString : PokerCard -> String
toString card =
    case card of
        RankedPokerSuited { rank, suit, facing } ->
            case facing of
                Down ->
                    "Pilatch PokerCard"

                Up ->
                    (rankToString rank) ++ " of " ++ (PokerSuit.toString suit)
