module Clock exposing (..)

import Time


type alias Clock =
    { zone : Time.Zone
    , time : Time.Posix
    }


inNL : Time.Posix -> Clock
inNL =
    Clock <| Time.customZone (2 * 60) []


toString : Clock -> String
toString clock =
    let
        pad =
            String.padLeft 2 '0'

        hour =
            String.fromInt <| toHour clock

        minute =
            String.fromInt <| toMinute clock
    in
    pad hour
        ++ ":"
        ++ pad minute


withTime : Clock -> Time.Posix -> Clock
withTime clock time =
    { clock | time = time }


toYear : Clock -> Int
toYear { zone, time } =
    Time.toYear zone time


toMonth : Clock -> Time.Month
toMonth { zone, time } =
    Time.toMonth zone time


toDay : Clock -> Int
toDay { zone, time } =
    Time.toDay zone time


toWeekday : Clock -> Time.Weekday
toWeekday { zone, time } =
    Time.toWeekday zone time


toHour : Clock -> Int
toHour { zone, time } =
    Time.toHour zone time


toMinute : Clock -> Int
toMinute { zone, time } =
    Time.toMinute zone time


toSecond : Clock -> Int
toSecond { zone, time } =
    Time.toSecond zone time


toMillis : Clock -> Int
toMillis { zone, time } =
    Time.toMillis zone time


toPosixMillis : Clock -> Int
toPosixMillis { time } =
    Time.posixToMillis time


addMinutes : Clock -> Int -> Clock
addMinutes clock minutes =
    let
        minutesInMillis =
            minutes * 60 * 1000
    in
    mapMillis clock ((+) minutesInMillis)


mapMillis : Clock -> (Int -> Int) -> Clock
mapMillis { time, zone } f =
    let
        millis =
            Time.posixToMillis time
    in
    { zone = zone, time = Time.millisToPosix <| f millis }
