module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Clock exposing (Clock)
import Context exposing (Context, Events)
import Debug exposing (todo)
import Event exposing (Event)
import Html exposing (Html)
import Http
import Json.Decode
import Lineup
import Platform exposing (Task)
import Task
import Time
import Url exposing (Url)



-- MODEL


type Model
    = Initial Url Nav.Key
    | Lineup Lineup.Model
    | Error String



-- MSG


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotTime Time.Posix
    | GotInitialTime Time.Zone Time.Posix
    | GotData (Result Http.Error (List Event))
    | UpdateEvent Event.Id Event.Status
    | LineupMsg Lineup.Msg



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Initial url _ ->
            { title = "Sniester 2024"
            , body = [ Html.p [] [ Html.text <| Url.toString url ] ]
            }

        Lineup m ->
            { title = "Sniester 2024"
            , body = [ Html.text "Lineup:", Html.map LineupMsg <| Lineup.view m ]
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
            let
                ctx =
                    Context key url (Clock Time.utc (Time.millisToPosix 0)) (Events events events events) Context.Friday
            in
            ( Lineup <| Lineup.new ctx, Cmd.none )

        ( GotData (Err err), _ ) ->
            ( Error <| Debug.toString err, Cmd.none )

        ( LineupMsg m, Lineup mdl ) ->
            ( Lineup <| Lineup.update m mdl, Cmd.none )

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
