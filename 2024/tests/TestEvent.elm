module TestEvent exposing (..)

import Event
import Expect
import Json.Decode as Dec
import Test exposing (..)


suite : Test
suite =
    test "decode"
        (\_ -> Expect.ok <| Dec.decodeString Event.decoder """{"name": "Magnetic Spacemen", "venue": "De Poot", "starttime": "2024-05-24T18:30:00+02:00", "endtime": "2024-05-24T19:15:00+02:00"}""")
