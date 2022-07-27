module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput, on, keyCode)
import Json.Decode as Decode

---- MODEL ----

type alias Model =
  { displayMain : Bool
  , newTodoText : String
  , editing : Bool
  , oldTodo : TodoList
  }

type alias TodoList = List (TodoItem)

type alias TodoItem =
  { description : String
  , completed : Bool
  , editing : Bool
  }

init : ( Model, Cmd Msg )
init =
  ( { displayMain = True
    , newTodoText = ""
    , editing = False
    , oldTodo = []
    }
  , Cmd.none
  )

---- UPDATE ----

type Msg
  = NoOp
  | ToggleMain
  | ToggleItem TodoItem
  | DeleteItem TodoItem
  | EditItem TodoItem
  | EditItemText TodoItem Bool String
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
      ( { model | oldTodo = replaceTodoItem todo { todo | completed = not todo.completed } model.oldTodo }, Cmd.none )

    DeleteItem todo ->
      ( { model | oldTodo = deleteTodoItem todo model.oldTodo }, Cmd.none )

    EditItem todo ->
      ( { model | oldTodo = replaceTodoItem todo { todo | editing = not todo.editing } model.oldTodo }, Cmd.none )

    EditItemText todo continueEditing desc ->
      let
        newTodo = { todo | description = desc, editing = continueEditing }
      in
        ( { model | oldTodo = replaceTodoItem todo newTodo model.oldTodo }, Cmd.none )

    ChangeNewTodo newTodo ->
      ( { model | newTodoText = newTodo }, Cmd.none )

    SubmitNewTodo ->
      ( { model | oldTodo = model.oldTodo ++ [ TodoItem model.newTodoText False False ], newTodoText = "" }, Cmd.none )

replaceTodoItem : TodoItem -> TodoItem -> TodoList -> TodoList
replaceTodoItem todo withTodo list =
  let
    replace : TodoItem -> TodoItem -> TodoItem
    replace todo1 todo2 =
      if todo1.description == todo2.description then
        withTodo
      else
        todo2
  in
    List.map (replace todo) list

deleteTodoItem : TodoItem -> TodoList -> TodoList
deleteTodoItem todo list =
  let
    remove todo1 =
      not (todo1.description == todo.description)
  in
    List.filter remove list

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
            , value model.newTodoText
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
        [ ul [ class "todo-list" ] ( viewTodoList model.oldTodo )
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

viewTodoList : TodoList -> List (Html Msg)
viewTodoList todos =
  List.map
    (viewTodoItem
      (\todo -> ToggleItem todo)
      (\todo -> DeleteItem todo)
      (\todo ->  EditItem todo)
    )
    todos

viewTodoItem : (TodoItem -> Msg) -> (TodoItem -> Msg) -> (TodoItem -> Msg) -> TodoItem -> Html Msg
viewTodoItem toggleItem deleteItem editItem todo =
  let
    desc = todo.description
    handleDoubleClick = if not todo.editing then (editItem todo) else NoOp
  in
    li
      [ classList
        [ ( "completed", todo.completed )
        , ( "editing", todo.editing)
        ]
      , onDoubleClick handleDoubleClick
      ]
      [ div
          [ class "view" ]
          [ input
              [ class "toggle"
              , type_ "checkbox"
              , checked todo.completed
              , onClick (toggleItem todo)
              ] []
          , label [] [text todo.description ]
          , button
              [ class "destroy"
              , onClick (deleteItem todo)
              ] []
          ]
      , input
          [ class "edit"
          , value desc
          , onInput (EditItemText todo True)
          , onEnter (EditItemText todo False desc)
          ] []
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


---- UTILITIES ----

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
