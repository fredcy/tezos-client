module View.Peers exposing (view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Http
import RemoteData exposing (RemoteData)
import Data.Chain as Chain
import View.Field exposing (formatDate, shortHash)


view : RemoteData Http.Error (List Chain.Peer) -> Html msg
view peersData =
    H.div []
        [ H.h3 [] [ H.text "Peers" ]
        , case peersData of
            RemoteData.Success peers ->
                viewPeersList peers

            _ ->
                H.text (toString peersData)
        ]


viewPeersList : List Chain.Peer -> Html msg
viewPeersList peers =
    let
        row peer =
            H.tr []
                [ H.td [ HA.class "hash" ] [ H.text (shortHash peer.hash) ]
                , H.td [] [ H.text peer.info.state ]
                , H.td [] [ H.text (toString peer.info.trusted) ]
                , H.td [] [ H.text (toString peer.info.score) ]
                , H.td [ HA.class "number" ] [ H.text (toString peer.info.stat.total_sent) ]
                , H.td [ HA.class "number" ] [ H.text (toString peer.info.stat.total_recv) ]
                , H.td [] [ viewConnection peer.info.lastConnection ]

                --, H.td [] [ H.text (toString peer.info.value) ]
                ]

        colHeader label =
            H.th [] [ H.text label ]

        thead =
            H.thead []
                (List.map colHeader
                    [ "id", "state", "trusted", "score", "sent", "recv", "last connection" ]
                )

        tbody =
            H.tbody [] (List.map row peers)
    in
        H.table [ HA.class "peers" ] [ thead, tbody ]


viewConnection : Maybe Chain.Connection -> Html msg
viewConnection connectionM =
    case connectionM of
        Just connection ->
            H.div [ HA.class "connection" ]
                [ viewAddress connection.addr
                , H.text " "
                , H.span [ HA.class "timestamp" ] [ H.text (formatDate connection.time) ]
                ]

        Nothing ->
            H.text ""


viewAddress : Chain.Address -> Html msg
viewAddress address =
    H.span []
        [ H.text (simplifyHost address.addr)
        , H.text ":"
        , H.text (toString address.port_)
        ]


simplifyHost : String -> String
simplifyHost host =
    let
        prefix =
            "::ffff:"
    in
        if String.startsWith prefix host then
            String.dropLeft (String.length prefix) host
        else
            host
