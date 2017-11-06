module View.Accounts exposing (view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Table
import Data.Chain exposing (AccountSummary)
import Update exposing (Msg(SetTableState, SetQuery))
import View.Field as VF
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
        }


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
            , footer
            ]


footer : Html msg
footer =
    H.div [ HA.class "footer" ]
        [ H.hr [] []
        , H.p []
            [ H.text """This displays all accounts that have participated in at
            least one transaction operation as source or destination. It does
            not include implicit transactions that occur within a contract."""
            ]
        ]


tezColumn : String -> (AccountSummary -> Int) -> Table.Column AccountSummary msg
tezColumn name toTez =
    Table.customColumn
        { name = name
        , viewData = \data -> formatSum (toTez data)
        , sorter = Table.increasingOrDecreasingBy toTez
        }


intColumn : String -> (AccountSummary -> Int) -> Table.Column AccountSummary msg
intColumn name toInt =
    Table.customColumn
        { name = name
        , viewData = \data -> formatCount (toInt data)
        , sorter = Table.increasingOrDecreasingBy toInt
        }


hashColumn : String -> (AccountSummary -> String) -> Table.Column AccountSummary msg
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


viewHash : String -> Table.HtmlDetails msg
viewHash hash =
    Table.HtmlDetails [ HA.class "hash" ]
        [ H.a
            [ Route.href (Route.Account hash) ]
            [ H.text hash ]
        ]
