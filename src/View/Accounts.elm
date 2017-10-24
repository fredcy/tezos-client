module View.Accounts exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Table
import Data.Chain exposing (AccountSummary)
import Update exposing (Msg, Msg(SetTableState))
import View.Field as VF


config : Table.Config AccountSummary Msg
config =
    Table.config
        { toId = .hash
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Hash" .hash
            , Table.stringColumn "sCount" (.sourceCount >> formatCount)
            , Table.stringColumn "sSum" (.sourceSum >> formatSum)
            , Table.stringColumn "dCount" (.destCount >> formatCount)
            , Table.stringColumn "dSum" (.destSum >> formatSum)
            ]
        }


view : Table.State -> List AccountSummary -> Html Msg
view tableState accounts =
    H.div [ HA.class "accounts-table-container" ]
        [ Table.view config tableState accounts ]


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
