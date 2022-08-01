module Model exposing (..)

import Json.Decode as Decode exposing (Decoder, bool, field, string, succeed)
import Json.Decode.Extra exposing (andMap, withDefault)



---- MODEL ----


type alias Model =
    { displayMain : Bool
    , newTodoText : String
    , todoList : TodoList
    , visibility : String
    }


type alias TodoList =
    List TodoItem


type alias TodoItem =
    { description : String
    , completed : Bool
    , editing : Bool
    }



---- Decoders & Encoders ----


modelDecoder : Decoder Model
modelDecoder =
    succeed Model
        |> andMap (field "displayMain" bool |> withDefault True)
        |> andMap (field "newTodoText" string |> withDefault "")
        |> andMap (field "todoList" todoListDecoder |> withDefault [])
        |> andMap (field "visibility" string |> withDefault "")


todoListDecoder : Decoder TodoList
todoListDecoder =
    Decode.list
        todoItemDecoder


todoItemDecoder : Decoder TodoItem
todoItemDecoder =
    succeed TodoItem
        |> andMap (field "description" string)
        |> andMap (field "completed" bool)
        |> andMap (field "editing" bool)
