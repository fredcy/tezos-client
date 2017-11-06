module Data.Request exposing (URL, emptyJsonBody)

import Http
import Json.Encode as Encode


type alias URL =
    String


emptyJsonBody : Http.Body
emptyJsonBody =
    Encode.object [] |> Http.jsonBody
