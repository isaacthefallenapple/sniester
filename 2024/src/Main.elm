module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Clock exposing (Clock)
import Context exposing (Context, Events, Schedule(..))
import Debug exposing (todo)
import Event exposing (Event)
import Html exposing (Html)
import Html.Attributes exposing (checked, class, classList, href, type_)
import Html.Events exposing (onClick)
import Http
import Iso8601
import Json.Decode as Dec
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Enc
import Lineup
import Platform exposing (Task)
import Ports
import Task
import Time
import Url exposing (Url)



-- MODEL


type Model
    = Lineup Lineup.Model
    | Error Nav.Key Url String



-- MSG


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotTime Time.Posix
    | GotInitialTime Time.Zone Time.Posix
    | UpdateEvent Event.Id Event.Status
    | LineupMsg Lineup.Msg
    | DayToggled Context.Schedule
    | CurrentTime Time.Posix



-- VIEW


marquee : Html msg -> Html msg
marquee content =
    let
        marqueeContent =
            Html.div
                [ class "marquee-content"
                ]
                [ content
                ]
    in
    Html.div
        [ class "marquee"
        ]
        [ marqueeContent
        , marqueeContent
        ]


viewUpNext : Maybe Event -> Html msg
viewUpNext maybeEvent =
    case maybeEvent of
        Nothing ->
            Html.div [] []

        Just event ->
            Html.article
                [ class "up-next-event" ]
                [ Html.div
                    [ class "up-next-eyebrow" ]
                    [ Html.text "Up Next" ]
                , marquee <|
                    Html.h3
                        [ class "up-next-name" ]
                        [ Html.text event.name ]
                , Html.span
                    [ class "up-next-time" ]
                    [ Html.text <| Clock.toString (Clock.inNL event.starttime) ]
                , Html.span
                    []
                    [ Html.text " @ " ]
                , Html.span
                    [ class "up-next-venue" ]
                    [ Html.text event.venue ]
                ]


viewSkeleton : Context -> (msg -> Msg) -> Html msg -> Document Msg
viewSkeleton ctx map html =
    let
        schedule =
            ctx.schedule

        title =
            Context.scheduleToString schedule
    in
    { title = "Sniester 2024" ++ " // " ++ title
    , body =
        [ Html.main_
            []
            [ Html.section
                [ class "up-next" ]
                [ viewUpNext <| Context.upNext ctx
                ]
            , Html.section
                [ class "select-view" ]
                []
            , Html.section
                [ class "day-toggle-container" ]
                [ viewSaturdayToggle schedule
                ]
            , Html.section
                [ class "view" ]
                [ Html.map map html
                ]
            , Html.nav
                [ class "nav" ]
                [ Html.a
                    [ href "#friday"
                    , classList [ ( "you-are-here", schedule == Friday ) ]
                    ]
                    [ Html.text "Friday"
                    ]
                , Html.a
                    [ href "#saturday"
                    , classList [ ( "you-are-here", schedule /= Friday ) ]
                    ]
                    [ Html.text "Saturday" ]
                ]
            ]
        ]
    }


viewSaturdayToggle : Context.Schedule -> Html Msg
viewSaturdayToggle schedule =
    if schedule == Friday then
        Html.div [] []

    else
        Html.div
            [ class "day-toggle"
            , onClick <|
                DayToggled <|
                    if schedule == Saturday then
                        Popup

                    else
                        Saturday
            ]
            [ Html.div [] [ Html.text "Regular" ]
            , Html.input
                [ type_ "checkbox"
                , checked <| schedule == Popup
                ]
                []
            , Html.div [ class "day-toggle-indicator" ]
                [ Html.div [] []
                ]
            , Html.div [] [ Html.text "Popup" ]
            ]


view : Model -> Document Msg
view model =
    case model of
        Lineup m ->
            viewSkeleton m.ctx LineupMsg <| Lineup.view m

        Error _ _ err ->
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

                        Error k _ _ ->
                            k
            in
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( DayToggled schedule, Lineup { ctx } ) ->
            ( Lineup <| Lineup.new <| Context.setSchedule ctx schedule, Nav.pushUrl ctx.key <| Context.scheduleToPath schedule )

        ( ChangedUrl ({ path, fragment } as url), Lineup { ctx } ) ->
            let
                schedule =
                    case fragment of
                        Just "saturday" ->
                            Saturday

                        Just "popup" ->
                            Popup

                        Just "friday" ->
                            Friday

                        _ ->
                            Context.todaysSchedule ctx.clock

                newCtx =
                    Context.setSchedule { ctx | url = url } schedule
            in
            ( Lineup <| Lineup.new newCtx, Cmd.none )

        ( LineupMsg m, Lineup mdl ) ->
            let
                ( newModel, cmds ) =
                    Lineup.update m mdl
            in
            ( Lineup newModel, Cmd.map LineupMsg cmds )

        ( CurrentTime currentTime, Lineup mdl ) ->
            let
                clock =
                    Clock.inNL currentTime

                minute =
                    Clock.toMinute clock

                fakeTime =
                    Iso8601.toTime ("2024-05-24T18:" ++ String.padLeft 2 '0' (String.fromInt minute) ++ ":00+02:00")
                        |> Result.withDefault currentTime

                _ =
                    Debug.log "fakeTime" (Clock.toString clock)

                ( newModel, cmds ) =
                    Lineup.update (Lineup.CurrentTime fakeTime) mdl
            in
            ( Lineup <| newModel, Cmd.map LineupMsg cmds )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (60 * 1000) CurrentTime



-- INIT


init : Dec.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsJson url key =
    let
        flags =
            Dec.decodeValue flagsDecoder flagsJson
    in
    case flags of
        Err err ->
            ( Error key url <| Debug.toString err, Cmd.none )

        Ok { time, friday, saturday, popup } ->
            let
                clock =
                    Clock.inNL <| Time.millisToPosix time

                schedule =
                    Context.todaysSchedule clock

                ctx =
                    Context key url clock (Events friday saturday popup) schedule
            in
            ( Lineup <| Lineup.new ctx, Nav.replaceUrl key (Context.scheduleToPath schedule) )


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
