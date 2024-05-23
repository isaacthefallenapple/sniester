module Lineup exposing (..)

import Clock exposing (Clock)
import Context exposing (Context, getScheduled)
import Event exposing (Event)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, checked, class, classList, name, title, type_)
import Html.Events exposing (onClick)
import List
import Ports
import Time


type Msg
    = ClickedEvent Event.Id
    | UpdateEvent Event.Id Event.Status
    | CurrentTime Time.Posix


type alias Model =
    { ctx : Context
    , selected : Maybe Event.Id
    }


new : Context -> Model
new ctx =
    { ctx = ctx
    , selected = Nothing
    }


type alias ToMsg msg =
    Event.Id -> Event.Status -> msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg ({ ctx } as model) =
    case msg of
        ClickedEvent event ->
            ( { model | selected = Just event }, Cmd.none )

        UpdateEvent id status ->
            let
                newCtx =
                    Context.setStatus ctx id status
            in
            ( { model | ctx = newCtx }, Ports.setStorage <| Context.encodeEvents newCtx )

        CurrentTime currentTime ->
            ( { model | ctx = Context.setTime ctx currentTime }, Cmd.none )


view : Model -> Html Msg
view { ctx, selected } =
    let
        currentTime =
            ctx.clock

        events =
            getScheduled ctx

        withTime =
            Clock.withTime currentTime

        ( startTime, endTime ) =
            Tuple.mapBoth withTime withTime <| findStartAndEnd events

        quarterHours =
            totalQuarterHours startTime endTime

        byVenue =
            eventsByVenue events

        indicator =
            if not <| Clock.isBetween startTime endTime currentTime then
                Nothing

            else
                Just <| toFloat (Clock.duration startTime currentTime) / toFloat (Clock.duration startTime (Clock.addMinutes endTime 15))
    in
    Html.div
        [ class "lineup-container" ]
        ((Html.div
            [ class "lineup"
            ]
          <|
            [ Html.div [ class "blackout-box" ] []
            , viewTimeline startTime quarterHours
            , viewVenues <| List.map .venue byVenue
            , viewEvents selected byVenue startTime currentTime (List.length byVenue) (quarterHours + 1) indicator
            ]
         )
            :: Maybe.withDefault [] (Maybe.map (viewUpdater >> List.singleton) (Maybe.andThen (Context.getEvent ctx) selected))
        )


viewEvent : Bool -> Clock -> Clock -> Int -> Event -> Html Msg
viewEvent selected startTime currentTime row { name, id, status, starttime, endtime } =
    let
        eventStart =
            Clock.withTime startTime starttime

        eventEnd =
            Clock.withTime startTime endtime

        quarterHours =
            totalQuarterHours startTime eventStart

        length =
            totalQuarterHours eventStart eventEnd

        isPast =
            Clock.toPosixMillis eventEnd <= Clock.toPosixMillis currentTime
    in
    Html.article
        [ class "event"
        , classList [ ( "selected", selected ), ( "past", isPast ) ]
        , class <| String.toLower <| Event.statusToString <| status
        , style
            [ ( "grid-row", String.fromInt (row + 1) )
            , ( "grid-column-start", String.fromInt (quarterHours + 1) )
            , ( "grid-column-end", String.fromInt (quarterHours + 1 + length) )
            ]
        , title name
        , onClick <| ClickedEvent id
        ]
        [ renderName name length
        ]


renderName : String -> Int -> Html msg
renderName name length =
    let
        longest =
            longestWordLength name

        canFit =
            length
                > 1
                && (longest + length - 1)
                // length
                <= 3
                && ((String.length name + 0) // 6)
                <= length
                && wordCount name
                <= length
                && name
                /= "The Northern Boys"

        rotate =
            length == 1
    in
    Html.span
        [ classList [ ( "small", not canFit ), ( "rotate", rotate ) ] ]
        [ Html.text name ]


wordCount : String -> Int
wordCount =
    String.split " " >> List.length


longestWordLength : String -> Int
longestWordLength str =
    str
        |> String.split " "
        |> List.map String.length
        |> List.maximum
        |> Maybe.withDefault 0


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


isSelected : Maybe Event.Id -> Event -> Bool
isSelected id e =
    id
        |> Maybe.map ((==) e.id)
        |> Maybe.withDefault False


viewEvents : Maybe Event.Id -> List { venue : String, events : List Event } -> Clock -> Clock -> Int -> Int -> Maybe Float -> Html Msg
viewEvents selectedId events startTime currentTime rows columns timeIndicator =
    Html.div
        [ class "events"
        , classList
            [ ( "time-indicator"
              , timeIndicator
                    |> Maybe.map (\_ -> True)
                    |> Maybe.withDefault False
              )
            ]
        , style <|
            [ ( "--rows", String.fromInt rows )
            , ( "--columns", String.fromInt columns )
            ]
                ++ (timeIndicator
                        |> Maybe.map (\p -> ( "--time-indicator-position", String.fromFloat (p * 100.0) ++ "%" ))
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                   )
        ]
    <|
        (events
            |> List.map .events
            |> List.indexedMap (\i -> List.map (\e -> viewEvent (isSelected selectedId e) startTime currentTime i e))
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


viewUpdater : Event -> Html Msg
viewUpdater ev =
    let
        data =
            [ Event.Going
            , Event.Interested
            , Event.Undecided
            , Event.Skip
            ]
    in
    Html.div
        [ class "event-info" ]
        [ Html.div
            [ class "event-name" ]
            [ Html.text ev.name ]
        , Html.div
            [ class "status-updater" ]
          <|
            List.map
                (\s ->
                    Html.button
                        [ classList [ ( "current-status", s == ev.status ) ]
                        , onClick <| UpdateEvent ev.id s
                        ]
                        [ Html.text <| Event.statusToEmoji s ]
                )
                data
        ]


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
