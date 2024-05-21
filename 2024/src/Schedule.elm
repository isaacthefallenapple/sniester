module Lineup exposing (..)

import Clock exposing (Clock)
import Event exposing (Event)
import Html exposing (Html)
import List exposing (map)
import Time


type alias ToMsg msg =
    Event.Id -> Event.Status -> msg


view : ToMsg msg -> List Event -> Clock -> Html msg
view toMsg events ({ zone, time } as clock) =
    let
        withTime =
            Clock.withTime clock

        ( startTime, endTime ) =
            Tuple.mapBoth withTime withTime <| findStartAndEnd events

        totalQuarters =
            totalQuarterHours startTime endTime
    in
    Html.text ""


viewEvent : ToMsg msg -> Event -> Html msg
viewEvent _ event =
    Html.article
        []
        [ Html.h3
            []
            [ Html.text event.name ]
        ]


viewTimeline : Clock -> Int -> Html msg
viewTimeline startTime quarterHours =
    let
        timestamps =
            List.map (\q -> Clock.addMinutes startTime <| q * 15) <| List.range 0 quarterHours
    in
    Html.header [] []


findStartAndEnd : List Event -> ( Time.Posix, Time.Posix )
findStartAndEnd events =
    let
        unwrap =
            Maybe.withDefault (Debug.todo "Maybe was Nothing")
    in
    ( Time.millisToPosix << unwrap <| List.minimum (List.map (.starttime >> Time.posixToMillis) events)
    , Time.millisToPosix << unwrap <| List.maximum (List.map (.endtime >> Time.posixToMillis) events)
    )


totalQuarterHours : Clock -> Clock -> Int
totalQuarterHours start end =
    let
        startMinute =
            Clock.toPosixMillis start // 1000 // 60

        endMinute =
            Clock.toPosixMillis end // 1000 // 60
    in
    (endMinute - startMinute) // 15
