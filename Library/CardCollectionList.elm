module Library.CardCollectionList exposing (add, CardCollectionList, empty, flip, pick, shuffle, size, take, toString)

import Library.Card as Card exposing (..)
import List
import Random


type alias CardCollectionList =
    List Card


add : CardCollectionList -> Card -> CardCollectionList
add cards card =
    card :: cards


empty : CardCollectionList
empty =
    []


flip : CardCollectionList -> CardCollectionList
flip cards =
    List.map Card.flip cards


pick : CardCollectionList -> Int -> Maybe Card
pick cards index =
    if index < 0 then
        Nothing
    else
        List.drop index cards |> List.head


shuffle : Random.Seed -> CardCollectionList -> ( CardCollectionList, Random.Seed )
shuffle seed cards =
    shuffle_ seed cards empty


size : CardCollectionList -> Int
size cards =
    List.length cards


take : CardCollectionList -> Int -> ( CardCollectionList, Maybe Card )
take cards index =
    if index < 0 then
        ( cards, Nothing )
    else
        let
            front =
                List.take index cards

            back =
                List.drop (index + 1) cards

            card =
                pick cards index
        in
            ( List.append front back, card )


toString : CardCollectionList -> List String
toString cards =
    List.map Basics.toString cards



-- PRIVATE --


shuffle_ : Random.Seed -> CardCollectionList -> CardCollectionList -> ( CardCollectionList, Random.Seed )
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


seededIndex_ : Random.Seed -> CardCollectionList -> ( Int, Random.Seed )
seededIndex_ seed cards =
    Random.step (size cards - 1 |> Random.int 0) seed
