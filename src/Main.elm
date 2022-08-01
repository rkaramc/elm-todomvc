port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onBlur, onClick, onDoubleClick, onInput)
import Json.Decode as Decode
import Model exposing (Model, TodoItem, TodoList, modelDecoder)


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue modelDecoder flags of
        Err _ ->
            Debug.todo "unable to decode model from localstorage!"

        Ok model ->
            ( model, Cmd.none )


type Msg
    = NoOp
    | ToggleMain
    | ToggleItem TodoItem
    | DeleteItem TodoItem
    | EditItem TodoItem
    | EditItemText TodoItem Bool String
    | ChangeNewTodoText String
    | SubmitNewTodo
    | SetVisibility String
    | ClearCompleted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleMain ->
            ( { model
                | displayMain =
                    not model.displayMain
              }
            , Cmd.none
            )

        SetVisibility visibility ->
            ( { model
                | visibility =
                    visibility
              }
            , Cmd.none
            )

        ChangeNewTodoText newTodoText ->
            ( { model
                | newTodoText =
                    newTodoText
              }
            , Cmd.none
            )

        SubmitNewTodo ->
            ( { model
                | todoList =
                    model.todoList ++ [ TodoItem model.newTodoText False False ]
                , newTodoText =
                    ""
              }
            , Cmd.none
            )

        ToggleItem todo ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.map
                            (\a ->
                                if todo.description == a.description then
                                    { todo | completed = not todo.completed }

                                else
                                    a
                            )
              }
            , Cmd.none
            )

        EditItem todo ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.map
                            (\a ->
                                if todo.description == a.description then
                                    { todo | editing = not todo.editing }

                                else
                                    a
                            )
              }
            , Cmd.none
            )

        EditItemText todo continueEditing desc ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.map
                            (\a ->
                                if todo.description == a.description then
                                    { todo | description = desc, editing = continueEditing }

                                else
                                    a
                            )
              }
            , Cmd.none
            )

        DeleteItem todo ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.filter (\a -> a.description /= todo.description)
              }
            , Cmd.none
            )

        ClearCompleted ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.filter (\a -> not a.completed)
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    section [ class "todoapp" ]
        [ header [ class "header " ]
            [ h1 [] [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , autofocus True
                , value model.newTodoText
                , onInput ChangeNewTodoText
                , onEnter SubmitNewTodo
                ]
                []
            , input [ class "toggle-all", id "toggle-all", type_ "checkbox", onClick ToggleMain ] []
            , label [ for "toggle-all" ] [ text "Toggle display of all Todos" ]
            ]
        , section
            [ classList
                [ ( "main", True )
                , ( "hidden", not model.displayMain )
                ]
            ]
            [ ul [ class "todo-list" ] (viewTodoList model.todoList model.visibility)
            , viewFooterActions (List.length model.todoList) model.visibility
            , viewFooterInfo
            ]
        ]


viewFooterActions : Int -> String -> Html Msg
viewFooterActions itemCount visibility =
    let
        textCount =
            String.fromInt itemCount
                ++ (if itemCount /= 1 then
                        " items"

                    else
                        " item"
                   )
    in
    footer [ class "footer" ]
        [ span [ class "todo-count" ] [ strong [] [ text textCount ] ]
        , ul [ class "filters" ]
            [ filterButton visibility "" "All"
            , filterButton visibility "active" "Active"
            , filterButton visibility "completed" "Completed"
            ]
        , button [ class "clear-completed", onClick ClearCompleted ] [ text "Clear completed" ]
        ]


filterButton : String -> String -> String -> Html Msg
filterButton visibility filter filterText =
    li []
        [ a
            [ classList [ ( "selected", visibility == filter ) ], onClick (SetVisibility filter) ]
            [ text filterText ]
        ]


viewFooterInfo : Html Msg
viewFooterInfo =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p [] [ text "Template by ", a [ href "http://sindresorhus.com" ] [ text "Sindre Sorhus" ] ]
        , p [] [ text "Created by ", a [ href "http://karamch.com/about" ] [ text "Rajeev K" ] ]
        , p [] [ text "Part of ", a [ href "http://todomvc.com" ] [ text "TodoMVC" ] ]
        ]


viewTodoList : TodoList -> String -> List (Html Msg)
viewTodoList todos visibility =
    let
        todoFilter =
            case visibility of
                "active" ->
                    \a -> not a.completed

                "completed" ->
                    \a -> a.completed

                _ ->
                    \_ -> True
    in
    todos
        |> List.filter todoFilter
        |> List.map
            (viewTodoItem
                (\todo -> ToggleItem todo)
                (\todo -> DeleteItem todo)
                (\todo -> EditItem todo)
            )


viewTodoItem : (TodoItem -> Msg) -> (TodoItem -> Msg) -> (TodoItem -> Msg) -> TodoItem -> Html Msg
viewTodoItem toggleItem deleteItem editItem todo =
    let
        desc =
            todo.description

        handleDoubleClick =
            if not todo.editing then
                editItem todo

            else
                NoOp
    in
    li
        [ classList
            [ ( "completed", todo.completed )
            , ( "editing", todo.editing )
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
                ]
                []
            , label [] [ text todo.description ]
            , button
                [ class "destroy"
                , onClick (deleteItem todo)
                ]
                []
            ]
        , input
            [ class "edit"
            , value desc
            , onInput (EditItemText todo True)
            , onEnter (EditItemText todo False desc)
            , onBlur (EditItemText todo False desc)
            ]
            []
        ]



---- PROGRAM ----


main : Program Decode.Value Model Msg
main =
    Browser.document
        { view = \model -> { title = "Elm • TodoMVC", body = [ view model ] }
        , init = init
        , update = updateWithStorage
        , subscriptions = always Sub.none
        }


port setStorage : Model -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



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
