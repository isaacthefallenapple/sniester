module Lineup exposing (..)

import Clock exposing (Clock)
import Context exposing (Context, getScheduled)
import Event exposing (Event)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, checked, class, classList, name, title, type_)
import Html.Events exposing (onClick)
import List
import Time


type Msg
    = ClickedEvent Event.Id
    | UpdateEvent Event.Id Event.Status


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


update : Msg -> Model -> Model
update msg ({ ctx } as model) =
    case msg of
        ClickedEvent event ->
            { model | selected = Just event }

        UpdateEvent id status ->
            { model | ctx = Context.setStatus ctx id status }


view : Model -> Html Msg
view { ctx, selected } =
    let
        events =
            getScheduled ctx

        withTime =
            Clock.withTime ctx.clock

        ( startTime, endTime ) =
            Tuple.mapBoth withTime withTime <| findStartAndEnd events

        quarterHours =
            totalQuarterHours startTime endTime

        byVenue =
            eventsByVenue events
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
            , viewEvents byVenue startTime (List.length byVenue) (quarterHours + 1)
            ]
         )
            :: Maybe.withDefault [] (Maybe.map (viewUpdater >> List.singleton) (Maybe.andThen (Context.getEvent ctx) selected))
        )


viewEvent : Clock -> Int -> Event -> Html Msg
viewEvent startTime row { name, id, status, starttime, endtime } =
    let
        eventStart =
            Clock.withTime startTime starttime

        eventEnd =
            Clock.withTime startTime endtime

        quarterHours =
            totalQuarterHours startTime eventStart

        length =
            totalQuarterHours eventStart eventEnd
    in
    Html.article
        [ class "event"
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


viewEvents : List { venue : String, events : List Event } -> Clock -> Int -> Int -> Html Msg
viewEvents events startTime rows columns =
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
            |> List.indexedMap (\i -> List.map (viewEvent startTime i))
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
