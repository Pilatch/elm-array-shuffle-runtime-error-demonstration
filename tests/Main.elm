port module Main exposing (..)

import Library.PerformanceTest.ShuffleTests as ShuffleTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit ShuffleTests.all


port emit : ( String, Value ) -> Cmd msg
