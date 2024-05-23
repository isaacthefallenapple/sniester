module Context exposing (..)

import Browser.Navigation as Nav
import Clock exposing (Clock)
import Event exposing (Event)
import Json.Encode as Enc
import Time
import Url exposing (Url)


type alias Context =
    { key : Nav.Key
    , url : Url
    , clock : Clock
    , events : Events
    , schedule : Schedule
    }


type alias Events =
    { friday : List Event
    , saturday : List Event
    , popup : List Event
    }


getScheduled : Context -> List Event
getScheduled { events, schedule } =
    events
        |> (case schedule of
                Friday ->
                    .friday

                Saturday ->
                    .saturday

                Popup ->
                    .popup
           )


setScheduled : Context -> List Event -> Context
setScheduled ({ schedule, events } as ctx) newEvents =
    case schedule of
        Friday ->
            { ctx | events = { events | friday = newEvents } }

        Saturday ->
            { ctx | events = { events | saturday = newEvents } }

        Popup ->
            { ctx | events = { events | popup = newEvents } }


updateScheduled : Context -> Event.Id -> (Event -> Event) -> Context
updateScheduled ctx id f =
    let
        schedule =
            getScheduled ctx
    in
    setScheduled ctx <|
        List.map
            (\e ->
                if e.id == id then
                    f e

                else
                    e
            )
            schedule


setSchedule : Context -> Schedule -> Context
setSchedule ctx schedule =
    { ctx | schedule = schedule }


setStatus : Context -> Event.Id -> Event.Status -> Context
setStatus ctx id status =
    updateScheduled ctx id (\e -> { e | status = status })


getEvent : Context -> Event.Id -> Maybe Event
getEvent ctx id =
    find (.id >> (==) id) (getScheduled ctx)


find : (a -> Bool) -> List a -> Maybe a
find f list =
    case list of
        [] ->
            Nothing

        x :: rest ->
            if f x then
                Just x

            else
                find f rest


type Schedule
    = Friday
    | Saturday
    | Popup


scheduleToString : Schedule -> String
scheduleToString schedule =
    case schedule of
        Friday ->
            "Friday"

        Saturday ->
            "Saturday"

        Popup ->
            "Popup"


setTime : Context -> Time.Posix -> Context
setTime ({ clock } as ctx) newTime =
    { ctx | clock = Clock.withTime clock newTime }


scheduleToPath : Schedule -> String
scheduleToPath =
    scheduleToString >> String.toLower >> (++) "/2024/"


encodeEvents : Context -> Enc.Value
encodeEvents ctx =
    Enc.object
        [ ( "friday", Enc.list Event.encode ctx.events.friday )
        , ( "saturday", Enc.list Event.encode ctx.events.saturday )
        , ( "popup", Enc.list Event.encode ctx.events.popup )
        ]
