module Event exposing (Event, Id, Status(..), decoder, encode, statusToEmoji, statusToString)

import Iso8601
import Json.Decode as Dec exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Enc exposing (Value)
import Time


type Status
    = Going
    | Interested
    | Skip
    | Undecided


statusToEmoji : Status -> String
statusToEmoji status =
    case status of
        Going ->
            "✅"

        Interested ->
            "👀"

        Skip ->
            "❌"

        Undecided ->
            "🤷"


statusToString : Status -> String
statusToString status =
    case status of
        Going ->
            "Going"

        Interested ->
            "Interested"

        Skip ->
            "Skip"

        Undecided ->
            "Undecided"


encodeStatus : Status -> Value
encodeStatus =
    statusToString >> Enc.string


decodeStatus : Decoder Status
decodeStatus =
    Dec.string
        |> Dec.andThen
            (\str ->
                Dec.succeed <|
                    case str of
                        "Going" ->
                            Going

                        "Interested" ->
                            Interested

                        "Skip" ->
                            Skip

                        _ ->
                            Undecided
            )


type alias Event =
    { name : String
    , venue : String
    , starttime : Time.Posix
    , endtime : Time.Posix
    , status : Status
    , id : Id
    }


mkEvent : String -> String -> Time.Posix -> Time.Posix -> Status -> Event
mkEvent name venue starttime endtime status =
    { name = name
    , venue = venue
    , starttime = starttime
    , endtime = endtime
    , status = status
    , id = mkId name starttime
    }


encode : Event -> Value
encode event =
    Enc.object
        [ ( "name", Enc.string event.name )
        , ( "venue", Enc.string event.venue )
        , ( "starttime", Enc.string <| Iso8601.fromTime event.starttime )
        , ( "endtime", Enc.string <| Iso8601.fromTime event.endtime )
        , ( "status", encodeStatus event.status )
        ]


decoder : Decoder Event
decoder =
    Dec.succeed mkEvent
        |> Pipeline.required "name" Dec.string
        |> Pipeline.required "venue" Dec.string
        |> Pipeline.required "starttime" Iso8601.decoder
        |> Pipeline.required "endtime" Iso8601.decoder
        |> Pipeline.optional "status" decodeStatus Undecided


type Id
    = Id String


mkId : String -> Time.Posix -> Id
mkId name time =
    let
        slug =
            name ++ (String.fromInt <| Time.posixToMillis time)
    in
    String.filter Char.isAlphaNum slug |> Id
