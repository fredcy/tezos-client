module View.Delegations exposing (view)

import Html as H exposing (Html)
import Html.Attributes as HA
import RemoteData
import Table
import Data.Chain exposing (DelegationSummary)
import Model exposing (Model)
import Msg exposing (Msg(SetDelegationsTableState))
import Route
import View.Field as VF


view : Model -> Html Msg
view model =
    case model.chain.delegations of
        RemoteData.Success delegations ->
            view2 model.delegationsTableState delegations

        _ ->
            H.text (toString model.chain.delegations)


view2 : Table.State -> List DelegationSummary -> Html Msg
view2 tableState delegations =
    H.div [ HA.class "delegations-container" ]
        [ Table.view config tableState delegations ]


config : Table.Config DelegationSummary Msg
config =
    Table.config
        { toId = .source
        , toMsg = SetDelegationsTableState
        , columns =
            [ idHashColumn "Source" .source
            , idHashColumn "Delegate" .delegate
            , Table.stringColumn "Timestamp" (.timestamp >> VF.formatDate)
            , blockHashColumn "Block" .blockHash
            ]
        }


idHashColumn : String -> (DelegationSummary -> String) -> Table.Column DelegationSummary msg
idHashColumn name toHash =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewIdHash (toHash data)
        , sorter = Table.increasingOrDecreasingBy toHash
        }


viewIdHash : String -> Table.HtmlDetails msg
viewIdHash hash =
    Table.HtmlDetails [ HA.class "hash" ]
        [ H.span
            [ HA.title hash ]
            [ H.text (VF.shortHash hash) ]
        ]


blockHashColumn : String -> (DelegationSummary -> String) -> Table.Column DelegationSummary msg
blockHashColumn name toHash =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewBlockHash (toHash data)
        , sorter = Table.increasingOrDecreasingBy toHash
        }


viewBlockHash : String -> Table.HtmlDetails msg
viewBlockHash hash =
    Table.HtmlDetails [ HA.class "hash" ]
        [ H.a
            [ Route.href (Route.Account hash) ]
            [ H.text (VF.shortHash hash) ]
        ]
