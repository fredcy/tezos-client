module View.Peers exposing (view)

import Date exposing (Date)
import Html as H exposing (Html)
import Html.Attributes as HA
import Http
import RemoteData exposing (RemoteData)
import Table
import Data.Chain as Chain
import Update exposing (Msg(SetPeerTableState))
import View.Field exposing (formatDate, shortHash)


view : RemoteData Http.Error (List Chain.Peer) -> Table.State -> Html Msg
view peersData tableState =
    H.div []
        [ H.h3 [] [ H.text "Peers" ]
        , case peersData of
            RemoteData.Success peers ->
                H.div [ HA.class "peers" ]
                    [ viewSortable peers tableState
                    , footer
                    ]

            _ ->
                H.text (toString peersData)
        ]


nullDate =
    Date.fromTime 0


type alias PeerView =
    { host : String
    , time : String
    , state : String
    , sent : Int
    , received : Int
    , id : String
    , rawValue : String
    }


toView : Chain.Peer -> PeerView
toView peer =
    { host =
        peer.info.lastConnection
            |> Maybe.map (.addr >> .addr >> simplifyHost)
            |> Maybe.withDefault ""
    , time =
        peer.info.lastConnection
            |> Maybe.map (.time >> formatDate)
            |> Maybe.withDefault ""
    , state = peer.info.state
    , sent = peer.info.stat.total_sent
    , received = peer.info.stat.total_recv
    , id = peer.hash
    , rawValue = toString peer.info.value
    }


config : Table.Config PeerView Msg
config =
    Table.config
        { toId = .id
        , toMsg = SetPeerTableState
        , columns =
            [ Table.stringColumn "node addr" .host
            , Table.stringColumn "latest connection" .time
            , Table.stringColumn "state" .state
            , Table.intColumn "sent" .sent
            , Table.intColumn "rcvd" .received
            , hashColumn "id" .id

            --, Table.stringColumn "raw value" .rawValue
            ]
        }


viewSortable : List Chain.Peer -> Table.State -> Html Msg
viewSortable peers tableState =
    peers
        |> List.map toView
        |> List.filter (\p -> p.host /= "")
        |> Table.view config tableState


hashColumn : String -> (a -> String) -> Table.Column a msg
hashColumn name toHash =
    let
        hashDetails hash =
            Table.HtmlDetails [ HA.class "hash" ] [ H.text (shortHash hash) ]
    in
        Table.veryCustomColumn
            { name = name
            , viewData = toHash >> hashDetails
            , sorter = Table.increasingOrDecreasingBy toHash
            }


footer : Html msg
footer =
    H.div [ HA.class "footer" ]
        [ H.hr [] []
        , H.p []
            [ H.text """This reports the peers known by a single node in the network. Peers that are somehow known by the node but have never connected are not displayed.""" ]
        ]



-- TODO remove dead code: viewPeersList and its unique dependencies


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
