module Api exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode


type alias OperationGroup =
    { hash : String
    , source : String
    }


requestBlockOperations : String -> String -> Http.Request (List OperationGroup)
requestBlockOperations nodeUrl blockHash =
    Http.get (nodeUrl ++ "/api/block/" ++ blockHash ++ "/operations") (Decode.list decodeOperationGroup)


decodeOperationGroup : Decode.Decoder OperationGroup
decodeOperationGroup =
    Decode.succeed OperationGroup
        |> Decode.required "Hash" Decode.string
        |> Decode.required "Source" Decode.string
