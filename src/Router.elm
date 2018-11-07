module Router exposing (Program, RouteChange(..), application)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key, load, pushUrl, replaceUrl)
import Html
import Json.Decode exposing (Decoder, Value)
import Url exposing (Url)


type Model model
    = Model Key model


type alias Program flags model msg =
    Platform.Program flags (Model model) (Msg msg)


type Msg msg
    = AppMsg msg
    | LinkClicked UrlRequest
    | UrlChanged Url


type RouteChange route
    = Push route
    | Replace route


application :
    { init : flags -> route -> ( model, Cmd msg, Maybe (RouteChange route) )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg, Maybe (RouteChange route) )
    , subscriptions : model -> Sub msg
    , stepUrl : route -> model -> ( model, Cmd msg, Maybe (RouteChange route) )
    , routeToString : route -> String
    , urlToRoute : Url -> route
    }
    -> Program flags model msg
application app =
    Browser.application
        { init = init app
        , view = view app
        , update = update app
        , subscriptions = subscriptions app
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init :
    { a
        | init : flags -> route -> ( model, Cmd msg, Maybe (RouteChange route) )
        , urlToRoute : Url -> route
        , routeToString : route -> String
    }
    -> flags
    -> Url
    -> Key
    -> ( Model model, Cmd (Msg msg) )
init app flags url key =
    app.init flags (app.urlToRoute url)
        |> toUpdate key app.routeToString


view : { a | view : model -> Document msg } -> Model model -> Document (Msg msg)
view app (Model _ model) =
    let
        { title, body } =
            app.view model
    in
    { title = title
    , body = List.map (Html.map AppMsg) body
    }


update :
    { a
        | update : msg -> model -> ( model, Cmd msg, Maybe (RouteChange route) )
        , urlToRoute : Url -> route
        , stepUrl : route -> model -> ( model, Cmd msg, Maybe (RouteChange route) )
        , routeToString : route -> String
    }
    -> Msg msg
    -> Model model
    -> ( Model model, Cmd (Msg msg) )
update app msg (Model key model) =
    case msg of
        AppMsg appMsg ->
            app.update appMsg model
                |> toUpdate key app.routeToString

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( Model key model
                    , pushUrl key (Url.toString url)
                    )

                External href ->
                    ( Model key model
                    , load href
                    )

        UrlChanged url ->
            app.stepUrl (app.urlToRoute url) model
                |> toUpdate key app.routeToString


toUpdate : Key -> (route -> String) -> ( model, Cmd msg, Maybe (RouteChange route) ) -> ( Model model, Cmd (Msg msg) )
toUpdate key routeToString ( model, cmd, maybeRouteChange ) =
    let
        mappedCmd =
            Cmd.map AppMsg cmd

        batchCmd =
            case maybeRouteChange of
                Nothing ->
                    mappedCmd

                Just (Push route) ->
                    Cmd.batch [ mappedCmd, pushUrl key (routeToString route) ]

                Just (Replace route) ->
                    Cmd.batch [ mappedCmd, replaceUrl key (routeToString route) ]
    in
    ( Model key model, batchCmd )


subscriptions : { a | subscriptions : model -> Sub msg } -> Model model -> Sub (Msg msg)
subscriptions app (Model _ model) =
    Sub.map AppMsg (app.subscriptions model)
