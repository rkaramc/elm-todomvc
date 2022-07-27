module Tests exposing (..)

import Test exposing (..)
import Expect
import Main exposing (TodoItem)


-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
  let
    item = TodoItem "hello" True
  in
  describe "A Test Suite"
    [ test "Todo Item has a description" <|
        \_ ->
          Expect.equal "hello" item.description
    , test "Todo Item has a completed flag" <|
        \_ ->
          Expect.equal True item.completed
    ]
