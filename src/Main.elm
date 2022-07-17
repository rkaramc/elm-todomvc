module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)
import Html.Events exposing (on, keyCode)
import Json.Decode as Decode


---- MODEL ----


type alias Model =
  { displayMain : Bool
  , newTodo : String
  , oldTodo : List TodoItem
  }

type alias TodoItem =
  { description : String
  , completed : Bool
  }

init : ( Model, Cmd Msg )
init =
  ( { displayMain = True
    , newTodo = ""
    , oldTodo = []
    }
  , Cmd.none
  )



---- UPDATE ----


type Msg
  = NoOp
  | ToggleMain
  | ToggleItem TodoItem
  | ChangeNewTodo String
  | SubmitNewTodo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case Debug.log "MESSAGE: " msg of
    NoOp ->
      ( model, Cmd.none )

    ToggleMain ->
      ( { model | displayMain = not model.displayMain }, Cmd.none )

    ToggleItem todo ->
      ( { model | oldTodo = toggleTodoItem { todo | completed = not todo.completed } model.oldTodo }, Cmd.none )

    ChangeNewTodo newTodo ->
      ( { model | newTodo = newTodo }, Cmd.none )

    SubmitNewTodo ->
      ( { model | oldTodo = model.oldTodo ++ [ TodoItem model.newTodo False ], newTodo = "" }, Cmd.none )


toggleTodoItem : TodoItem -> List (TodoItem) -> List (TodoItem)
toggleTodoItem todo list =
  let
    replace : TodoItem -> TodoItem -> TodoItem
    replace todo1 todo2 =
      if todo1.description == todo2.description then
        todo1
      else
        todo2
  in
    List.map (replace todo) list

---- VIEW ----


view : Model -> Html Msg
view model =
  section [ class "todoapp"]
    [ header [ class "header "]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value model.newTodo
            , onInput ChangeNewTodo
            , onEnter SubmitNewTodo
        ] []
        , input [ class "toggle-all", id "toggle-all", type_ "checkbox", onClick ToggleMain ] []
        , label [ for "toggle-all"] [ text "Toggle display of all Todos" ]
        ]
    , section
        [ classList
            [ ("main", True)
            , ("hidden", model.displayMain)
            ]
        ]
        [ ul [ class "todo-list" ] ( todoList model.oldTodo )
        , footer [ class "footer" ]
            [ span [ class "todo-count" ] [ strong [] [ text "0" ], text " item left" ]
            , ul [ class "filters" ]
                [ li [] [ a [ class "selected", href "#/" ] [ text "All" ] ]
                , li [] [ a [ href "#/active" ] [ text "Active" ] ]
                , li [] [ a [ href "#/completed" ] [ text "Completed" ] ]
                ]
            , button [ class "clear-completed" ] [ text "Clear completed" ]
            ]
        , footer [ class "info" ]
            [ p [] [ text "Double-click to edit a todo" ]
            , p [] [ text "Template by ", a [ href "http://sindresorhus.com" ] [ text "Sindre Sorhus" ]]
            , p [] [ text "Created by ", a [ href "http://karamch.com/about" ] [ text "Rajeev K" ]]
            , p [] [ text "Part of ", a [ href "http://todomvc.com" ] [ text "TodoMVC" ]]
            ]
        ]
    ]

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    isEnter code =
      if code == 13 then
        Decode.succeed msg
      else
        Decode.fail "not ENTER"
  in
    on "keydown" (Decode.andThen isEnter keyCode)



todoList : List (TodoItem) -> List (Html Msg)
todoList todos =
  List.map todoListItem todos

todoListItem : TodoItem -> Html Msg
todoListItem todo =
  li
    [ classList
        [ ( "completed", todo.completed ) ]
    ]
    [ div
        [ class "view" ]
        [ input
            [ class "toggle"
            , type_ "checkbox"
            , checked todo.completed
            , onClick (ToggleItem todo)
            ] []
        , label [] [text todo.description ]
        , button [ class "destroy" ] []
        ]
    , input [ class "edit", value todo.description ] []
    ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
