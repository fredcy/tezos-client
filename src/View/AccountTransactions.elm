module View.AccountTransactions exposing (view)

import Date
import Html as H exposing (Html)
import Html.Attributes as HA
import Table
import Data.Chain as Chain exposing (TransactionSummary)
import Update exposing (Msg(SetTransactionTableState))
import View.Field as VF exposing (formatDate, shortHash)
import Route


config : Chain.AccountID -> Table.Config TransactionSummary Msg
config accountHash =
    Table.config
        { toId = .timestamp >> toString
        , toMsg = SetTransactionTableState
        , columns =
            [ timeColumn "timestamp" .timestamp .block
            , hashColumn "source" .source
            , hashColumn "destination" .destination
            , amountColumn "amount" accountHash .amount .destination
            ]
        }


view : Chain.AccountID -> Table.State -> List TransactionSummary -> Html Msg
view accountHash tableState transactions =
    H.div [ HA.class "transactions-container" ]
        [ Table.view (config accountHash) tableState transactions
        ]


amountColumn : String -> Chain.AccountID -> (TransactionSummary -> Int) -> (TransactionSummary -> String) -> Table.Column TransactionSummary msg
amountColumn name account toTez toDestination =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewAmountColumn account (toTez data) (toDestination data)
        , sorter = Table.increasingOrDecreasingBy toTez
        }


viewAmountColumn : Chain.AccountID -> Int -> String -> Table.HtmlDetails msg
viewAmountColumn account tez destination =
    let
        class =
            if account == destination then
                "destination"
            else
                "source"
    in
        Table.HtmlDetails [ HA.class class ]
            [ H.text (formatSum tez) ]


timeColumn : String -> (TransactionSummary -> Date.Date) -> (TransactionSummary -> String) -> Table.Column TransactionSummary msg
timeColumn name toTimestamp toHash =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> timestampLink (toTimestamp data) (toHash data)
        , sorter = Table.increasingOrDecreasingBy (toTimestamp >> toString)
        }


timestampLink : Date.Date -> String -> Table.HtmlDetails msg
timestampLink timestamp blockHash =
    Table.HtmlDetails []
        [ H.a
            [ Route.href (Route.Block blockHash) ]
            [ H.text (formatDate timestamp) ]
        ]


hashColumn : String -> (TransactionSummary -> Chain.AccountID) -> Table.Column TransactionSummary msg
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


viewHash : Chain.AccountID -> Table.HtmlDetails msg
viewHash hash =
    Table.HtmlDetails [ HA.class "hash" ]
        [ H.a
            [ Route.href (Route.Account hash) ]
            [ H.text (shortHash hash) ]
        ]
