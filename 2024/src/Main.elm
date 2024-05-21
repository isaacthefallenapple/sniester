module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Clock exposing (Clock)
import Debug
import Event exposing (Event)
import Html
import Http
import Json.Decode
import Lineup
import Platform exposing (Task)
import Task
import Time
import Url exposing (Url)



-- MODEL


type alias Context =
    { key : Nav.Key
    , url : Url
    , timezone : Time.Zone
    , time : Time.Posix
    }


type alias Events =
    { friday : List Event
    , saturday : List Event
    , popup : List Event
    }


type EventView
    = LineUp
    | Schedule
    | Venue


type Model
    = Initial Url Nav.Key
    | Model
        { ctx : Context
        , events : Events
        , view : EventView
        }
    | Error String



-- MSG


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotTime Time.Posix
    | GotInitialTime Time.Zone Time.Posix
    | GotData (Result Http.Error (List Event))
    | UpdateEvent Event.Id Event.Status



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Initial url _ ->
            { title = "Sniester 2024"
            , body = [ Html.p [] [ Html.text <| Url.toString url ] ]
            }

        Model { events, ctx } ->
            { title = "Sniester 2024"
            , body = [ Html.text "Lineup:", Lineup.view UpdateEvent (Debug.log "friday" events.friday) <| Clock ctx.timezone ctx.time ]
            }

        Error err ->
            { title = "error"
            , body = [ Html.text err ]
            }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, Initial _ key ) ->
            ( Initial url key, Cmd.none )

        ( GotData (Ok events), Initial url key ) ->
            ( Model { ctx = Context key url Time.utc (Time.millisToPosix 0), events = Events (Debug.log "events" events) events events, view = LineUp }, Cmd.none )

        ( GotData (Err err), _ ) ->
            ( Error <| Debug.toString err, Cmd.none )

        -- ( GotInitialTime zone now, Initial url key ) ->
        --     ( Model { ctx = { key = key, url = url, timezone = zone, time = now }, friday = [], saturday = [], popup = [] }, Cmd.none )
        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- INIT


getInitialTime : Task x ( Time.Zone, Time.Posix )
getInitialTime =
    Task.map2 Tuple.pair Time.here Time.now


getInitialData : Cmd Msg
getInitialData =
    Http.get
        { url = "data/data-saturday.json"
        , expect = Http.expectJson GotData (Json.Decode.list Event.decoder)
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    -- ( Initial url key, Task.perform (\( zone, now ) -> GotInitialTime zone now) getInitialTime )
    ( Initial url key, getInitialData )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
