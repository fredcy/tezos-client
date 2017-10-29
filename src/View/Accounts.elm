module View.Accounts exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Table
import Data.Chain as Chain exposing (AccountSummary)
import Update exposing (Msg, Msg(SetTableState, SetQuery))
import View.Field as VF exposing (formatDate, shortHash, formatCentiles)
import Route


config : Table.Config AccountSummary Msg
config =
    Table.config
        { toId = .hash
        , toMsg = SetTableState
        , columns =
            [ hashColumn "Hash" .hash
            , tezColumn "total sent (ꜩ)" .sourceSum
            , intColumn "# sent" .sourceCount
            , tezColumn "total rcvd (ꜩ)" .destSum
            , intColumn "# rcvd" .destCount
            ]

        --, customizations = customizations
        }


customizations =
    { tableAttrs = []
    , caption = Nothing
    , thead = makeThead
    , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = \data -> []
    }


makeThead : List ( String, Table.Status, H.Attribute Msg ) -> Table.HtmlDetails Msg
makeThead list =
    let
        makeTh ( name, status, attr ) =
            H.th [ attr ] [ H.text (name ++ " " ++ marker status) ]

        marker status =
            case status of
                Table.Reversible (Just False) ->
                    "v"

                Table.Reversible (Just True) ->
                    "^"

                _ ->
                    " "
    in
        Table.HtmlDetails []
            [ H.tr []
                [ H.th [] []
                , H.th [ HA.colspan 2 ] [ H.text "Source" ]
                , H.th [ HA.colspan 2 ] [ H.text "Destination" ]
                ]
            , H.tr [] (List.map makeTh list)
            ]


view : Table.State -> String -> List AccountSummary -> Html Msg
view tableState query accounts =
    let
        accountsToShow =
            if String.toLower query == query then
                List.filter (String.contains query << String.toLower << .hash) accounts
            else
                List.filter (String.contains query << .hash) accounts
    in
        H.div [ HA.class "accounts-table-container" ]
            [ H.input [ HA.placeholder "search by hash", HA.class "query", HE.onInput SetQuery ] []
            , Table.view config tableState accountsToShow
            ]


viewTransactions : Chain.AccountID -> List Chain.TransactionSummary -> Html Msg
viewTransactions accountHash transactions =
    let
        class : Chain.TransactionSummary -> String
        class t =
            if t.source == accountHash then
                "source"
            else if t.destination == accountHash then
                "destination"
            else
                ""

        thead =
            H.tr []
                [ H.th [] [ H.text "time" ]
                , H.th [] [ H.text "source" ]
                , H.th [] [ H.text "destination" ]
                , H.th [] [ H.text "amount (ꜩ)" ]
                ]

        row t =
            H.tr [ HA.class (class t) ]
                [ H.td [] [ H.a [ Route.href (Route.Block t.block) ] [ H.text (formatDate t.timestamp) ] ]
                , H.td [ HA.class "hash" ]
                    [ H.a [ Route.href (Route.Account t.source) ] [ H.text (shortHash t.source) ]
                    ]
                , H.td [ HA.class "hash" ]
                    [ H.a [ Route.href (Route.Account t.destination) ] [ H.text (shortHash t.destination) ] ]
                , H.td [ HA.class "number amount" ] [ H.text (formatCentiles t.amount) ]
                ]
    in
        H.div []
            [ H.h4 [] [ H.text "Transactions" ]
            , H.table [ HA.class "transactions" ]
                [ thead
                , H.tbody [] (List.map row transactions)
                ]
            ]


tezColumn name toTez =
    Table.customColumn
        { name = name
        , viewData = \data -> formatSum (toTez data)
        , sorter = Table.increasingOrDecreasingBy toTez
        }


intColumn name toInt =
    Table.customColumn
        { name = name
        , viewData = \data -> formatCount (toInt data)
        , sorter = Table.increasingOrDecreasingBy toInt
        }


hashColumn name toHash =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewHash (toHash data)
        , sorter = Table.increasingOrDecreasingBy toHash
        }


formatSum : Int -> String
formatSum i =
    case i of
        0 ->
            "."

        _ ->
            VF.formatCentiles i


formatCount : Int -> String
formatCount c =
    if c == 0 then
        "."
    else
        toString c


viewHash hash =
    Table.HtmlDetails [ HA.class "hash" ]
        [ H.a
            [ Route.href (Route.Account hash) ]
            [ H.text hash ]
        ]
