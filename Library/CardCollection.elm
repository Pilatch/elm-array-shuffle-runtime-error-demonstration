module Library.CardCollection exposing (add, append, CardCollection, empty, flip, fromList, pick, shuffle, size, take, toList, top, toString)

import Library.PokerCard as PokerCard exposing (..)
import Random
import Array exposing (Array)


type alias CardCollection =
    Array PokerCard


add : CardCollection -> PokerCard -> CardCollection
add cards card =
    Array.append (Array.fromList [ card ]) cards


append : CardCollection -> CardCollection -> CardCollection
append front back =
    Array.append front back


empty : CardCollection
empty =
    Array.empty


flip : CardCollection -> CardCollection
flip cards =
    Array.map PokerCard.flip cards


fromList : List PokerCard -> CardCollection
fromList cards =
    Array.fromList cards


pick : CardCollection -> Int -> Maybe PokerCard
pick cards index =
    Array.get index cards


shuffle : Random.Seed -> CardCollection -> ( CardCollection, Random.Seed )
shuffle seed cards =
    shuffle_ seed cards empty


size : CardCollection -> Int
size cards =
    Array.length cards


take : CardCollection -> Int -> ( CardCollection, Maybe PokerCard )
take cards index =
    if index < 0 then
        ( cards, Nothing )
    else
        let
            front =
                Array.slice 0 index cards

            back =
                Array.slice (index + 1) (size cards) cards

            card =
                Array.get index cards
        in
            ( Array.append front back, card )


toList : Array PokerCard -> List PokerCard
toList cards =
    Array.toList cards


top : CardCollection -> Int -> CardCollection
top cards howMany =
    Array.slice 0 howMany cards


toString : CardCollection -> List String
toString cards =
    Array.map Basics.toString cards |> Array.toList



-- PRIVATE --


shuffle_ : Random.Seed -> CardCollection -> CardCollection -> ( CardCollection, Random.Seed )
shuffle_ seed ordered shuffled =
    if size ordered == 0 then
        ( shuffled, seed )
    else
        let
            ( cardIndex, newSeed ) =
                seededIndex_ seed ordered

            ( newOrdered, maybeTakenCard ) =
                take ordered cardIndex
        in
            case maybeTakenCard of
                Just card ->
                    shuffle_ newSeed newOrdered (add shuffled card)

                Nothing ->
                    ( shuffled, seed )


seededIndex_ : Random.Seed -> CardCollection -> ( Int, Random.Seed )
seededIndex_ seed cards =
    Random.step (size cards - 1 |> Random.int 0) seed
