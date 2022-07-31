port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onBlur, onClick, onDoubleClick, onInput)
import Json.Decode as Decode exposing (Decoder, andThen, bool, field, string, succeed)
import Json.Decode.Extra as Decode exposing (..)



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


todoItemDecoder : Decoder TodoItem
todoItemDecoder =
    succeed TodoItem
        |> andMap (field "description" string)
        |> andMap (field "completed" bool)
        |> andMap (field "editing" bool)


todoListDecoder : Decoder TodoList
todoListDecoder =
    Decode.list
        todoItemDecoder


modelDecoder : Decoder Model
modelDecoder =
    succeed Model
        |> andMap (field "displayMain" bool |> withDefault True)
        |> andMap (field "newTodoText" string |> withDefault "")
        |> andMap (field "todoList" todoListDecoder |> withDefault [])
        |> andMap (field "visibility" string |> withDefault "")


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
    | ChangeNewTodo String
    | SubmitNewTodo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleMain ->
            ( { model | displayMain = not model.displayMain }, Cmd.none )

        ToggleItem todo ->
            ( { model
                | todoList =
                    replaceTodoItem
                        todo
                        { todo | completed = not todo.completed }
                        model.todoList
              }
            , Cmd.none
            )

        DeleteItem todo ->
            ( { model | todoList = deleteTodoItem todo model.todoList }, Cmd.none )

        EditItem todo ->
            ( { model
                | todoList =
                    replaceTodoItem
                        todo
                        { todo | editing = not todo.editing }
                        model.todoList
              }
            , Cmd.none
            )

        EditItemText todo continueEditing desc ->
            ( { model
                | todoList =
                    replaceTodoItem
                        todo
                        { todo | description = desc, editing = continueEditing }
                        model.todoList
              }
            , Cmd.none
            )

        ChangeNewTodo newTodo ->
            ( { model | newTodoText = newTodo }, Cmd.none )

        SubmitNewTodo ->
            ( { model | todoList = model.todoList ++ [ TodoItem model.newTodoText False False ], newTodoText = "" }, Cmd.none )


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
    section [ class "todoapp" ]
        [ header [ class "header " ]
            [ h1 [] [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , autofocus True
                , value model.newTodoText
                , onInput ChangeNewTodo
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
            [ ul [ class "todo-list" ] (viewTodoList model.todoList)
            , viewFooterActions (List.length model.todoList)
            , viewFooterInfo
            ]
        ]


viewFooterActions : Int -> Html Msg
viewFooterActions itemCount =
    let
        textCount =
            String.fromInt itemCount
                ++ (if itemCount /= 1 then
                        " items left"

                    else
                        " item left"
                   )
    in
    footer [ class "footer" ]
        [ span [ class "todo-count" ] [ strong [] [ text textCount ] ]
        , ul [ class "filters" ]
            [ li [] [ a [ class "selected", href "#/" ] [ text "All" ] ]
            , li [] [ a [ href "#/active" ] [ text "Active" ] ]
            , li [] [ a [ href "#/completed" ] [ text "Completed" ] ]
            ]
        , button [ class "clear-completed" ] [ text "Clear completed" ]
        ]


viewFooterInfo : Html Msg
viewFooterInfo =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p [] [ text "Template by ", a [ href "http://sindresorhus.com" ] [ text "Sindre Sorhus" ] ]
        , p [] [ text "Created by ", a [ href "http://karamch.com/about" ] [ text "Rajeev K" ] ]
        , p [] [ text "Part of ", a [ href "http://todomvc.com" ] [ text "TodoMVC" ] ]
        ]


viewTodoList : TodoList -> List (Html Msg)
viewTodoList todos =
    List.map
        (viewTodoItem
            (\todo -> ToggleItem todo)
            (\todo -> DeleteItem todo)
            (\todo -> EditItem todo)
        )
        todos


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
