module TestSpec exposing (..)

import Array exposing (..)
import Expect
import Fuzz exposing (int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Testing main"
        [ describe "test set mood"
            [ test "setMood updates with new mood" <|
                \() ->
                    let
                        record =
                            { mood = "10"
                            , energy = "10"
                            }
                    in
                    Expect.equal { mood = "5", energy = "10" } (Main.setMood "5" record)
            ]
        , describe "test set energy"
            [ test "setEnergy updates with new energy" <|
                \() ->
                    let
                        record =
                            { mood = "10"
                            , energy = "10"
                            }
                    in
                    Expect.equal { mood = "10", energy = "5" } (Main.setEnergy "5" record)
            ]
        , describe "test getPoint"
            [ test "getPoint function works with incorrect value" <|
                \() ->
                    let
                        array =
                            Array.fromList [] |> Array.get 0
                    in
                    Expect.equal { mood = "0", energy = "0" } (Main.getPoint array)
            , test "getPoint function works with correct value" <|
                \() ->
                    let
                        array =
                            Array.fromList [ { mood = "1", energy = "2" } ] |> Array.get 0
                    in
                    Expect.equal { mood = "1", energy = "2" } (Main.getPoint array)
            ]
        ]



-- describe "The String module"
--     [ describe "String.reverse"
--         -- Nest as many descriptions as you like.
--         [ test "has no effect on a palindrome" <|
--             \() ->
--                 let
--                     palindrome =
--                         "hannah"
--                 in
--                 Expect.equal palindrome (String.reverse palindrome)
--
--         -- Expect.equal is designed to be used in pipeline style, like this.
--         , test "reverses a known string" <|
--             \_ ->
--                 "ABCDEFG"
--                     |> String.reverse
--                     |> Expect.equal "GFEDCBA"
--
--         -- fuzz runs the test 100 times with randomly-generated inputs!
--         , fuzz string "restores the original string if you run it again" <|
--             \randomlyGeneratedString ->
--                 randomlyGeneratedString
--                     |> String.reverse
--                     |> String.reverse
--                     |> Expect.equal randomlyGeneratedString
--         ]
--     ]
