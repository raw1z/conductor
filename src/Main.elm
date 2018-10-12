module Pomodoro exposing (main)

import Browser
import Browser.Dom exposing (focus)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, placeholder, src, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, field, map, string)
import Page exposing (Page)
import Page.Tasks as Tasks
import Route exposing (Route)
import Task
import Timer
import Url exposing (Url)


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { currentPage : Page
    , url : Url
    , key : Nav.Key
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    changeRouteTo (Route.fromUrl url)
        { currentPage = Page.Tasks Tasks.initialModel
        , url = url
        , key = key
        }


viewPage : Model -> Html Msg
viewPage model =
    let
        show pageView pageModel pageMsg =
            pageView pageModel
                |> Html.map pageMsg
    in
    case model.currentPage of
        Page.Tasks tasksModel ->
            show Tasks.view tasksModel TasksMsg


view : Model -> Document Msg
view model =
    { title = "Conductor"
    , body =
        [ div [ class "app" ]
            [ viewPage model
            ]
        ]
    }


type Msg
    = TasksMsg Tasks.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


setCurrentPage : Model -> Page -> Model
setCurrentPage model page =
    { model | currentPage = page }


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        changeRouteToPage page pageMsg ( pageModel, pageCmd ) =
            ( setCurrentPage model (page pageModel)
            , Cmd.map pageMsg pageCmd
            )
    in
    case maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Tasks ->
            Tasks.init
                |> changeRouteToPage Page.Tasks TasksMsg


updatePage : Msg -> Model -> ( Model, Cmd Msg )
updatePage msg model =
    let
        updateWith page pageMsg ( updatedPageModel, pageCmd ) =
            ( setCurrentPage model (page updatedPageModel)
            , Cmd.map pageMsg pageCmd
            )
    in
    case ( msg, model.currentPage ) of
        ( TasksMsg tasksMsg, Page.Tasks tasksModel ) ->
            Tasks.update tasksMsg tasksModel
                |> updateWith Page.Tasks TasksMsg

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External _ ->
                    ( model, Cmd.none )

        UrlChanged url ->
            changeRouteTo (Route.fromUrl url) model

        _ ->
            updatePage msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentPage of
        Page.Tasks tasksModel ->
            Sub.map TasksMsg (Tasks.subscriptions tasksModel)
