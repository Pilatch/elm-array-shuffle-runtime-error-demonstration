module Library.PokerSuit exposing (PokerSuit(..), toString)

import List


type PokerSuit
    = Hearts
    | Clubs
    | Spades
    | Diamonds


toString : PokerSuit -> String
toString suit =
    case suit of
        Hearts ->
            "Hearts"

        Clubs ->
            "Clubs"

        Diamonds ->
            "Diamonds"

        Spades ->
            "Spades"
