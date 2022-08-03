module Tests exposing (..)

import Expect
import Json.Decode exposing (Error(..), Value, decodeString, decodeValue)
import Json.Encode as Encode
import Model exposing (Model, TodoItem, modelDecoder)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


model : Test
model =
    describe "Model"
        [ describe "decoder"
            [ test "decodes Model objects in json to elm" <|
                \_ ->
                    let
                        emptyJson =
                            "{}"

                        initialModel : Model
                        initialModel =
                            Model True "" [] ""
                    in
                    Expect.equal
                        (Ok initialModel)
                        (emptyJson |> decodeString modelDecoder)
            , test "decodes Model objects without visibility field" <|
                \_ ->
                    let
                        modelStringOld =
                            """{"displayMain": true, "newTodoText": "", "todoList": []}"""

                        initialModel : Model
                        initialModel =
                            Model True "" [] ""
                    in
                    Expect.equal
                        (Ok initialModel)
                        (modelStringOld |> decodeString modelDecoder)
            , test "decodes Model objects with todo list" <|
                \_ ->
                    let
                        modelStringWithTodoList =
                            """{
                                "displayMain": true,
                                "newTodoText": "",
                                "todoList": [
                                    {
                                        "description": "todo",
                                        "completed": false,
                                        "editing": false
                                    }
                                ],
                                "visibility": ""
                            }"""

                        modelValueWithTodoList : Model
                        modelValueWithTodoList =
                            Model True "" [ TodoItem "todo" False False ] ""
                    in
                    Expect.equal
                        (Ok modelValueWithTodoList)
                        (modelStringWithTodoList |> decodeString modelDecoder)
            , test "decodes elm values to Model objects" <|
                \_ ->
                    let
                        input : Value
                        input =
                            Encode.object []

                        output : Model
                        output =
                            Model True "" [] ""
                    in
                    Expect.equal
                        (Ok output)
                        (input |> decodeValue modelDecoder)
            ]
        ]
