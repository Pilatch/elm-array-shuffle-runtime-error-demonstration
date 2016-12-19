module Library.PerformanceTest.ShuffleTests exposing (..)

import Library.CardCollection as CardCollection
import Library.CardCollectionHamt as CardCollectionHamt exposing (..)
import Library.PokerDeck as PokerDeck exposing (..)
import Test exposing (..)
import Random
import Expect


-- Choose which Array implementation you want to test here.


chooseYourTest_ : TestType
chooseYourTest_ =
    Hamt



-- Change number of shuffles from one to two or more to reproduce the runtime error in core array.


numberOfShuffles_ : Int
numberOfShuffles_ =
    2


type TestType
    = Core
    | Hamt


all : Test
all =
    describe "Shuffle Performance Tests"
        tests_


tests_ : List Test
tests_ =
    case chooseYourTest_ of
        Hamt ->
            [ describe "Shuffle Array Hamt Performance Test"
                [ test (Basics.toString numberOfShuffles_ ++ " shuffles") <|
                    \() ->
                        let
                            deck =
                                PokerDeck.full |> PokerDeck.toList |> CardCollectionHamt.fromList
                        in
                            shuffleArrayHamtManyTimes_ (Random.initialSeed 1) deck numberOfShuffles_ |> Expect.equal 0
                ]
            ]

        Core ->
            [ describe "Shuffle Core Array Performance Test"
                [ test (Basics.toString numberOfShuffles_ ++ " shuffles") <|
                    \() -> shuffleArrayManyTimes_ (Random.initialSeed 1) PokerDeck.full numberOfShuffles_ |> Expect.equal 0
                ]
            ]


shuffleArrayManyTimes_ : Random.Seed -> PokerDeck -> Int -> Int
shuffleArrayManyTimes_ seed deck numberOfTimes =
    if numberOfTimes == 0 then
        0
    else
        let
            ( shuffledDeck, newSeed ) =
                CardCollection.shuffle seed deck
        in
            shuffleArrayManyTimes_ newSeed shuffledDeck (numberOfTimes - 1)


shuffleArrayHamtManyTimes_ : Random.Seed -> CardCollectionHamt -> Int -> Int
shuffleArrayHamtManyTimes_ seed deck numberOfTimes =
    if numberOfTimes == 0 then
        0
    else
        let
            ( shuffledDeck, newSeed ) =
                CardCollectionHamt.shuffle seed deck
        in
            shuffleArrayHamtManyTimes_ newSeed shuffledDeck (numberOfTimes - 1)
