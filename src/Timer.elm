port module Timer exposing (Model, Msg, init, resume, shift, stop, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import ProgressRing
import Time


port notify : String -> Cmd msg


type Statuts
    = Work Int
    | Pause Int
    | LongPause
    | Ready Int


type alias Model =
    { initialValue : Int
    , timeout : Int
    , status : Statuts
    , isActive : Bool
    }


init : Model
init =
    Model 0 0 (Ready 1) False



-- Utility methods


getLabel : Model -> String
getLabel model =
    if model.isActive then
        case model.status of
            Work _ ->
                "Working hard..."

            Pause _ ->
                "Take a breathe..."

            LongPause ->
                "Take some rest..."

            Ready _ ->
                "Ready to go"

    else
        "Ready to go"


stop : Model -> Model
stop model =
    case model.status of
        Work count ->
            { model
                | isActive = False
                , status = Ready count
            }

        _ ->
            model


resume : Model -> Model
resume model =
    case model.status of
        Ready count ->
            { model
                | isActive = True
                , status = Work count
            }

        _ ->
            model



-- VIEW


formatMinutesOrSeconds : Int -> String
formatMinutesOrSeconds value =
    String.padLeft 2 '0' (String.fromInt value)


viewTimerTimeout : Model -> Html Msg
viewTimerTimeout model =
    let
        remainingTime =
            model.initialValue - model.timeout

        minutes =
            remainingTime // 60

        seconds =
            remainingTime - (minutes * 60)
    in
    h1 [ class "timeout" ]
        [ text (formatMinutesOrSeconds minutes)
        , text ":"
        , text (formatMinutesOrSeconds seconds)
        ]


viewTimerProgress : Model -> Html Msg
viewTimerProgress model =
    let
        progress =
            if model.isActive then
                toFloat model.timeout / toFloat model.initialValue

            else
                0.0
    in
    div [ class "timer-progress" ]
        [ div [ class "timer-progress-ring" ]
            [ ProgressRing.viewProgress 150 6 progress ]
        , viewTimerTimeout model
        ]


viewTimerClassNames : Model -> String
viewTimerClassNames model =
    if model.isActive then
        case model.status of
            Work _ ->
                "timer timer-work"

            Pause _ ->
                "timer timer-pause"

            LongPause ->
                "timer timer-long-pause"

            Ready _ ->
                "timer timer-inactive"

    else
        "timer timer-inactive"


view : Model -> Html Msg
view model =
    div [ class (viewTimerClassNames model) ]
        [ viewTimerProgress model
        , div [ class "task-description" ]
            [ text <| getLabel model ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isActive then
        Time.every 1000 Tick

    else
        Sub.none



-- update


type Msg
    = Tick Time.Posix


shiftModel : Model -> Model
shiftModel model =
    case model.status of
        Ready count ->
            { model
                | status = Work count
                , initialValue = 1500
                , timeout = 0
                , isActive = True
            }

        Work 4 ->
            { model
                | status = LongPause
                , initialValue = 300
                , timeout = 0
                , isActive = True
            }

        Work count ->
            { model
                | status = Pause count
                , initialValue = 1200
                , timeout = 0
                , isActive = True
            }

        Pause count ->
            { model
                | status = Ready (count + 1)
                , initialValue = 0
                , timeout = 0
                , isActive = False
            }

        LongPause ->
            { model
                | status = Ready 1
                , initialValue = 0
                , timeout = 0
                , isActive = False
            }


shift : Model -> ( Model, Cmd Msg )
shift model =
    let
        newModel =
            shiftModel model
    in
    case model.status of
        Ready _ ->
            ( newModel, Cmd.none )

        _ ->
            ( newModel
            , notify <| getLabel newModel
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.timeout == model.initialValue then
                shift model

            else
                ( { model | timeout = model.timeout + 1 }
                , Cmd.none
                )
