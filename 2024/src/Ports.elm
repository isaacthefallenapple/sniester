port module Ports exposing (..)

import Json.Encode as Enc


port setStorage : Enc.Value -> Cmd msg


port scrollIntoView : String -> Cmd msg
