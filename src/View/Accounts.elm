module View.Accounts exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Table
import Data.Chain exposing (AccountSummary)
import Update exposing (Msg, Msg(SetTableState, SetQuery))
import View.Field as VF


config : Table.Config AccountSummary Msg
config =
    Table.config
        { toId = .hash
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Hash" .hash
            , intColumn "sCount" .sourceCount
            , tezColumn "sSum" .sourceSum
            , intColumn "dCount" .destCount
            , tezColumn "dSum" .destSum
            ]
        }


view : Table.State -> String -> List AccountSummary -> Html Msg
view tableState query accounts =
    let
        accountsToShow =
            List.filter (String.contains (String.toLower query) << String.toLower << .hash) accounts
    in
        H.div [ HA.class "accounts-table-container" ]
            [ H.input [ HA.placeholder "search by hash", HA.class "query", HE.onInput SetQuery ] []
            , Table.view config tableState accountsToShow
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
