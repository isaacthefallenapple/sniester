module Lineup exposing (..)

import Clock exposing (Clock)
import Event exposing (Event)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, title)
import List
import Time


type alias ToMsg msg =
    Event.Id -> Event.Status -> msg


view : ToMsg msg -> List Event -> Clock -> Html msg
view toMsg events ({} as clock) =
    let
        withTime =
            Clock.withTime clock

        ( startTime, endTime ) =
            Tuple.mapBoth withTime withTime <| findStartAndEnd events

        quarterHours =
            totalQuarterHours startTime endTime

        byVenue =
            eventsByVenue events
    in
    Html.div
        [ class "lineup"
        ]
        [ Html.div [ class "blackout-box" ] []
        , viewTimeline startTime quarterHours
        , viewVenues <| List.map .venue byVenue
        , viewEvents toMsg byVenue startTime (List.length byVenue) (quarterHours + 1)
        ]


viewEvent : ToMsg msg -> Clock -> Int -> Event -> Html msg
viewEvent _ startTime row event =
    let
        eventStart =
            Clock.withTime startTime event.starttime

        eventEnd =
            Clock.withTime startTime event.endtime

        quarterHours =
            totalQuarterHours startTime eventStart

        length =
            totalQuarterHours eventStart eventEnd
    in
    Html.article
        [ style
            [ ( "grid-row", String.fromInt (row + 1) )
            , ( "grid-column-start", String.fromInt (quarterHours + 1) )
            , ( "grid-column-end", String.fromInt (quarterHours + 1 + length) )
            ]
        , title event.name
        ]
        [ Html.h3
            []
            [ Html.text event.name ]
        ]


eventsByVenue : List Event -> List { venue : String, events : List Event }
eventsByVenue events =
    let
        unique =
            List.foldl
                (\x uniq ->
                    if List.member x uniq then
                        uniq

                    else
                        x :: uniq
                )
                []

        venues =
            events
                |> List.map .venue
                |> unique
                |> List.reverse

        byVenue =
            venues
                |> List.map (\v -> { venue = v, events = List.filter (.venue >> (==) v) events })
    in
    byVenue


viewTimeline : Clock -> Int -> Html msg
viewTimeline startTime quarterHours =
    let
        timestamps =
            List.map (\q -> Clock.addMinutes startTime <| q * 15) <| List.range 0 quarterHours
    in
    Html.header
        [ class "timestamps"
        , style [ ( "--columns", String.fromInt (quarterHours + 1) ) ]
        ]
    <|
        (timestamps
            |> List.map Clock.toString
            |> List.map (\s -> Html.div [] [ Html.text s ])
        )


viewVenues : List String -> Html msg
viewVenues venues =
    Html.header
        [ class "venues"
        , style [ ( "--rows", String.fromInt (List.length venues) ) ]
        ]
    <|
        List.map (\s -> Html.div [] [ Html.text s ]) venues


viewEvents : ToMsg msg -> List { venue : String, events : List Event } -> Clock -> Int -> Int -> Html msg
viewEvents toMsg events startTime rows columns =
    Html.div
        [ class "events"
        , style
            [ ( "--rows", String.fromInt rows )
            , ( "--columns", String.fromInt columns )
            ]
        ]
    <|
        (events
            |> List.map .events
            |> List.indexedMap (\i -> List.map (viewEvent toMsg startTime i))
            |> List.concat
        )


findStartAndEnd : List Event -> ( Time.Posix, Time.Posix )
findStartAndEnd events =
    let
        unwrap : (() -> a) -> Maybe a -> a
        unwrap f m =
            case m of
                Just x ->
                    x

                Nothing ->
                    f ()

        panic =
            unwrap <| \_ -> Debug.todo "Maybe was Nothing"
    in
    ( Time.millisToPosix << panic <| List.minimum (List.map (.starttime >> Time.posixToMillis) events)
    , Time.millisToPosix << panic <| List.maximum (List.map (.endtime >> Time.posixToMillis) events)
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


style : List ( String, String ) -> Attribute msg
style styles =
    let
        css =
            styles
                |> List.foldl
                    (\( prop, val ) acc ->
                        String.append acc (prop ++ ":" ++ val ++ ";\n")
                    )
                    ""
    in
    attribute "style" css
