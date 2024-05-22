module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Clock exposing (Clock)
import Context exposing (Context, Events, Schedule(..))
import Debug exposing (todo)
import Event exposing (Event)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, href)
import Http
import Json.Decode as Dec
import Json.Decode.Pipeline as Pipeline
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


viewSkeleton : Context.Schedule -> (msg -> Msg) -> Html msg -> Document Msg
viewSkeleton schedule map html =
    let
        title =
            case schedule of
                Friday ->
                    "Friday"

                Saturday ->
                    "Saturday"

                Popup ->
                    "Popup"
    in
    { title = "Sniester 2024" ++ " // " ++ title
    , body =
        [ Html.main_
            []
            [ Html.section
                [ class "up-next" ]
                []
            , Html.section
                [ class "select-view" ]
                []
            , Html.section
                [ class "view" ]
                [ Html.map map html
                ]
            , Html.nav
                [ class "nav" ]
                [ Html.a
                    [ href "friday"
                    , classList [ ( "you-are-here", schedule == Friday ) ]
                    ]
                    [ Html.text "Friday"
                    ]
                , Html.a
                    [ href "saturday"
                    , classList [ ( "you-are-here", schedule == Saturday ) ]
                    ]
                    [ Html.text "Saturday" ]
                , Html.a
                    [ href "popup"
                    , classList [ ( "you-are-here", schedule == Popup ) ]
                    ]
                    [ Html.text "Popup" ]
                ]
            ]
        ]
    }


view : Model -> Document Msg
view model =
    case model of
        Initial url _ ->
            { title = "Sniester 2024"
            , body = [ Html.p [] [ Html.text <| Url.toString url ] ]
            }

        Lineup m ->
            viewSkeleton m.ctx.schedule LineupMsg <| Lineup.view m

        Error err ->
            { title = "error"
            , body = [ Html.text err ]
            }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            let
                key =
                    case model of
                        Lineup m ->
                            m.ctx.key

                        Initial _ k ->
                            k

                        _ ->
                            Debug.todo "panic"
            in
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl ({ path, query } as url), Lineup { ctx } ) ->
            let
                schedule =
                    if String.endsWith "/saturday" path then
                        Saturday

                    else if String.endsWith "/popup" path then
                        Popup

                    else
                        Friday

                newCtx =
                    Context.setSchedule { ctx | url = url } schedule
            in
            ( Lineup <| Lineup.new newCtx, Cmd.none )

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
        , expect = Http.expectJson GotData (Dec.list Event.decoder)
        }


init : Dec.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsJson url key =
    let
        flags =
            Dec.decodeValue flagsDecoder flagsJson
    in
    case flags of
        Err err ->
            ( Error <| Debug.toString err, Cmd.none )

        Ok { time, friday, saturday, popup } ->
            let
                ctx =
                    Context key url (Clock Time.utc <| Time.millisToPosix time) (Events friday saturday popup) Friday
            in
            ( Lineup <| Lineup.new ctx, Cmd.none )



-- ( Initial url key, Task.perform (\( zone, now ) -> GotInitialTime zone now) getInitialTime )


main : Program Dec.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Flags =
    { time : Int
    , friday : List Event
    , saturday : List Event
    , popup : List Event
    }


flagsDecoder : Dec.Decoder Flags
flagsDecoder =
    Dec.succeed Flags
        |> Pipeline.required "time" Dec.int
        |> Pipeline.required "friday" (Dec.list Event.decoder)
        |> Pipeline.required "saturday" (Dec.list Event.decoder)
        |> Pipeline.required "popup" (Dec.list Event.decoder)
