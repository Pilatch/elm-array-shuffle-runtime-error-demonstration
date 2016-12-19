module Library.CardCollectionHamt exposing (add, append, CardCollectionHamt, empty, flip, fromList, pick, shuffle, size, take, toList, top, toString)

import Library.PokerCard as PokerCard exposing (..)
import Random
import Array.Hamt as Array exposing (Array)


type alias CardCollectionHamt =
    Array PokerCard


add : CardCollectionHamt -> PokerCard -> CardCollectionHamt
add cards card =
    Array.append (Array.fromList [ card ]) cards


append : CardCollectionHamt -> CardCollectionHamt -> CardCollectionHamt
append front back =
    Array.append front back


empty : CardCollectionHamt
empty =
    Array.empty


flip : CardCollectionHamt -> CardCollectionHamt
flip cards =
    Array.map PokerCard.flip cards


fromList : List PokerCard -> CardCollectionHamt
fromList cards =
    Array.fromList cards


pick : CardCollectionHamt -> Int -> Maybe PokerCard
pick cards index =
    Array.get index cards


shuffle : Random.Seed -> CardCollectionHamt -> ( CardCollectionHamt, Random.Seed )
shuffle seed cards =
    shuffle_ seed cards empty


size : CardCollectionHamt -> Int
size cards =
    Array.length cards


take : CardCollectionHamt -> Int -> ( CardCollectionHamt, Maybe PokerCard )
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


top : CardCollectionHamt -> Int -> CardCollectionHamt
top cards howMany =
    Array.slice 0 howMany cards


toString : CardCollectionHamt -> List String
toString cards =
    Array.map Basics.toString cards |> Array.toList



-- PRIVATE --


shuffle_ : Random.Seed -> CardCollectionHamt -> CardCollectionHamt -> ( CardCollectionHamt, Random.Seed )
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


seededIndex_ : Random.Seed -> CardCollectionHamt -> ( Int, Random.Seed )
seededIndex_ seed cards =
    Random.step (size cards - 1 |> Random.int 0) seed
