module View.Peers exposing (view)

import Date exposing (Date)
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
            let
                lastAddr : Maybe Chain.Connection -> Html msg
                lastAddr connectionMaybe =
                    connectionMaybe |> Maybe.map (.addr >> viewAddress) |> Maybe.withDefault (H.text "")

                lastTime connectionMaybe =
                    connectionMaybe |> Maybe.map (.time >> viewTime) |> Maybe.withDefault (H.text "")
            in
                H.tr []
                    [ H.td [ HA.class "hash" ] [ H.text (shortHash peer.hash) ]
                    , H.td [] [ H.text peer.info.state ]
                    , H.td [ HA.class "number" ] [ H.text (toString peer.info.stat.total_sent) ]
                    , H.td [ HA.class "number" ] [ H.text (toString peer.info.stat.total_recv) ]
                    , H.td [] [ lastAddr peer.info.lastConnection ]
                    , H.td [] [ lastTime peer.info.lastConnection ]

                    --, H.td [] [ H.text (toString peer.info.value) ]
                    ]

        colHeader label =
            H.th [] [ H.text label ]

        thead =
            H.thead []
                (List.map colHeader
                    [ "id", "state", "sent", "recv", "latest addr", "latest connection" ]
                )

        tbody =
            H.tbody [] (List.map row peers)
    in
        H.table [ HA.class "peers" ] [ thead, tbody ]


viewAddress : Chain.Address -> Html msg
viewAddress address =
    H.span []
        [ H.text (simplifyHost address.addr)
        , H.text ":"
        , H.text (toString address.port_)
        ]


viewTime : Date -> Html msg
viewTime time =
    H.span [ HA.class "timestamp" ] [ H.text (formatDate time) ]


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
