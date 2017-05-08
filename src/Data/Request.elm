module Data.Request exposing (..)

import Http
import Json.Encode as Encode


type alias URL =
    String


emptyJsonBody : Http.Body
emptyJsonBody =
    Encode.object [] |> Http.jsonBody
