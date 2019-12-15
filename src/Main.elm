module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Element
import Element.Background
import Element.Input
import Element.Font
import Time
import List.Extra


---- MODEL ----
type alias Model =
    {timer : Timer
    , tasks : Tasks}

type alias Tasks = List Task
type alias Task = {name : String, estimate : Maybe Minutes}

type Minutes = Minutes Int

type Timer =
    Preparation
    | Work Int
    | Break Int

init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )

initModel = Model Preparation []

countDownByOne : Model -> Model
countDownByOne model =
    case model.timer of
        Work secondsRemaining ->
            if secondsRemaining > 0
                then {model | timer = Work (secondsRemaining - 1)}
                else {model | timer = Break (toSeconds breakLength)}
        Break secondsRemaining ->
            if secondsRemaining > 0
                then {model | timer = Break (secondsRemaining - 1)}
                else {model | timer = Preparation}
        Preparation -> model

---- UPDATE ----


type Msg
    = Start
    | UpdateTask Int Task
    | CreateTask String
    | SecondPassed

workLength = Minutes 25
breakLength = Minutes 5

toSeconds (Minutes m) = m * 60

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mdl = case msg of
            SecondPassed -> countDownByOne model
            Start -> {model | timer = Work (toSeconds workLength)}
            UpdateTask index task -> {model | tasks = List.Extra.setAt index task model.tasks}
            CreateTask name -> {model | tasks = model.tasks ++ [{estimate = Nothing, name = name}]}
    in
        (mdl, Cmd.none)

viewTask : Task -> Element.Element Task
viewTask task =
        Element.row []
            [ viewNamePicker task.name |> Element.map (\newTitle -> {task | name = newTitle})
            , viewTimePicker task.estimate |> Element.map (\newEstimate -> {task | estimate = newEstimate})
            ]


viewNamePicker : String -> Element.Element String
viewNamePicker title =
    Element.Input.text
        []
        { onChange = identity
        , label = Element.Input.labelLeft [] (Element.text "Task:")
        , placeholder = Nothing
        , text = title
        }

startButton = Element.Input.button [] {label = Element.text "start", onPress = Just Start}

viewTimePicker : Maybe Minutes -> Element.Element (Maybe Minutes)
viewTimePicker estimate =
    case estimate of
        Just (Minutes n) -> Element.text (String.fromInt n)
        Nothing -> Element.text "no estimate set"


---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [interFont, Element.padding 10] (viewTimer model)

viewTimer : Model -> Element.Element Msg
viewTimer model =
    let
        timer = model.timer
    in
        case timer of
            Preparation -> viewPreparation model.tasks
            Work secondsRemaining -> Element.column [redBackground] [Element.text <| viewTime secondsRemaining]
            Break secondsRemaining -> Element.column [greenBackground] [Element.text <| viewTime secondsRemaining]

viewPreparation : Tasks -> Element.Element Msg
viewPreparation tasks =
    let
        instructions = Element.text "Jot down your todos"
    in
        Element.column [] [
            instructions
          , viewTasks tasks
          , startButton
        ]

viewTasks tasks =
    let
        listing =
            List.indexedMap (\i task -> viewTask task |> Element.map (UpdateTask i)) tasks

        -- Once the user gives the task a name, it's added to our list
        newTask = viewNamePicker ""
            |> Element.map CreateTask
    in
        Element.column [] (listing ++ [newTask])

interFont =
    Element.Font.family [ Element.Font.typeface "Inter", Element.Font.sansSerif ]


viewTime : Int -> String
viewTime totalSeconds =
    let
        minutes = floor (toFloat totalSeconds / 60)
        seconds = totalSeconds - minutes * 60
    in
        (String.fromInt minutes) ++ ":" ++ (String.fromInt seconds |> String.padLeft 2 '0')


redBackground = Element.Background.color (Element.rgb 1 0 0)
greenBackground = Element.Background.color (Element.rgb 0 1 0)

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }

subscriptions model =
    case model.timer of
        Preparation -> Sub.none
        Break secondsRemaining -> countDown
        Work secondsRemaining -> countDown

countDown =
        Time.every 1000 (always SecondPassed)
