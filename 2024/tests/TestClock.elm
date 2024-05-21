module TestClock exposing (..)

import Clock exposing (Clock)
import Expect
import Test exposing (..)
import Time


zero : Clock
zero =
    Clock Time.utc <| Time.millisToPosix 0


suite : Test
suite =
    describe "toString"
        [ test "zero" (\_ -> Expect.equal "00:00" <| Clock.toString zero)
        , test "nonzero" (\_ -> Expect.equal "00:15" <| Clock.toString <| Clock.addMinutes zero 15)
        ]
