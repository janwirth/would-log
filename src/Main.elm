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
import Element.Events
import Events


---- MODEL ----
type alias Model =
    {timer : Timer
    , tasks : Tasks
    , newTaskName : String
    }

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

initModel = Model Preparation [] ""

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
    | CreateTask
    | SetNewTaskName String
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
            CreateTask ->
                if String.isEmpty model.newTaskName
                    then model
                    else {model | tasks = model.tasks ++ [{estimate = Nothing, name = model.newTaskName}], newTaskName = ""}
            SetNewTaskName name -> {model | newTaskName = name}
    in
        (mdl, Cmd.none)

viewTask : Task -> Element.Element Task
viewTask task =
        Element.row []
            [ viewTaskNamePicker task.name |> Element.map (\newTitle -> {task | name = newTitle})
            , viewTimePicker task.estimate |> Element.map (\newEstimate -> {task | estimate = newEstimate})
            ]


viewNewTaskNamePicker : String -> Element.Element Msg
viewNewTaskNamePicker newTaskName =
    Element.Input.text
        [Events.onEnterKeyDown CreateTask]
        { onChange = SetNewTaskName
        , label = Element.Input.labelLeft [] (Element.text "Add Task:")
        , placeholder = Nothing
        , text = newTaskName
        }

viewTaskNamePicker : String -> Element.Element String
viewTaskNamePicker title =
    Element.Input.text
        []
        { onChange = identity
        , label = Element.Input.labelLeft [] (Element.text "Task:")
        , placeholder = Nothing
        , text = title
        }

startButton = Element.Input.button [Element.padding 15, lightGreyBackground] {label = Element.text "start", onPress = Just Start}

viewTimePicker : Maybe Minutes -> Element.Element (Maybe Minutes)
viewTimePicker estimate =
    let
        viewTimeOption t =
            let
                isSelected = estimate == (Just (Minutes t))
                selectedStyle =
                    if isSelected
                        then [lightGreyBackground]
                        else []
                onClick = Element.Events.onClick (if isSelected then Nothing else Just (Minutes t))
            in
            Element.el
                (onClick :: Element.padding 10 :: selectedStyle)
                (Element.text <| String.fromInt t)

    in
        List.map viewTimeOption timeOptions
        |> Element.row [Element.spacing 5]

timeOptions = [5, 10, 15, 20, 25]
lightGreyBackground = Element.Background.color (Element.rgba 0 0 0 0.05)

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
            Preparation -> viewPreparation model
            Work secondsRemaining -> Element.column [redBackground] [Element.text <| viewTime secondsRemaining]
            Break secondsRemaining -> Element.column [greenBackground] [Element.text <| viewTime secondsRemaining]

viewPreparation : Model  -> Element.Element Msg
viewPreparation model=
    let
        instructions = Element.text "Jot down your todos"
    in
        Element.column [] [
            instructions
          , viewTasks model
          , startButton
        ]

viewTasks : Model -> Element.Element Msg
viewTasks model =
    let
        tasks = model.tasks
        listing =
            List.indexedMap (\i task -> viewTask task |> Element.map (UpdateTask i)) tasks

        -- Once the user gives the task a name, it's added to our list
        newTaskName = viewNewTaskNamePicker model.newTaskName
        newTaskButton = Element.Input.button [Element.padding 15, lightGreyBackground] {onPress = Just CreateTask, label = Element.text "Create Task"}
    in
        Element.column [lightGreyBackground , Element.padding 10, Element.spacing 15] (listing ++ [newTaskName, newTaskButton])

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
