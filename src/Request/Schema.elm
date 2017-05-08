module Request.Schema exposing (..)

import Http
import Json.Encode as Encode
import Data.Request exposing (URL)
import Data.Schema as Schema


getSchema : URL -> String -> Http.Request Schema.SchemaData
getSchema nodeUrl schemaQuery =
    let
        body =
            [ ( "recursive", Encode.bool True ) ] |> Encode.object |> Http.jsonBody

        url =
            nodeUrl ++ schemaQuery
    in
        Http.post url body Schema.decodeSchema
