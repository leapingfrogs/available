port module Ports.Google exposing (..)

import Json.Encode


port onLogin : (Json.Encode.Value -> msg) -> Sub msg


port getToken : List String -> Cmd msg


port onToken : (String -> msg) -> Sub msg


port logout : () -> Cmd msg
