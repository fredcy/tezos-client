module View.Delegations exposing (view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import RemoteData
import Table
import Data.Chain exposing (DelegationSummary)
import Model exposing (Model)
import Msg exposing (Msg(SetDelegationsTableState, DelegationsFilter))
import Route
import View.Field as VF


view : Model -> Html Msg
view model =
    case model.chain.delegations of
        RemoteData.Success delegations ->
            view2 model.delegationsTableState model.delegationsFilter delegations

        _ ->
            H.text (toString model.chain.delegations)


view2 : Table.State -> String -> List DelegationSummary -> Html Msg
view2 tableState filter delegations =
    let
        match : String -> Bool
        match =
            if String.toLower filter == filter then
                String.contains filter << String.toLower
            else
                String.contains filter

        filterFn : DelegationSummary -> Bool
        filterFn d =
            match d.source || match d.delegate

        delegationsFiltered =
            if filter == "" then
                delegations
            else
                List.filter filterFn delegations
    in
        H.div [ HA.class "delegations-container" ]
            [ H.input [ HA.placeholder "filter", HA.class "query", HE.onInput DelegationsFilter ] []
            , Table.view config tableState delegationsFiltered
            , viewFooter
            ]


config : Table.Config DelegationSummary Msg
config =
    Table.config
        { toId = .source
        , toMsg = SetDelegationsTableState
        , columns =
            [ idHashColumn "Account" .source
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
            [ Route.href (Route.Block hash) ]
            [ H.text (VF.shortHash hash) ]
        ]


viewFooter : Html msg
viewFooter =
    H.div [ HA.class "footer" ]
        [ H.hr [] []
        , H.p [] [ H.text """This displays one row for every delegation operation.
It does not display default delegations for accounts, only those delegations recorded as an operation.
An account may have multiple delegates over time, the most recent being effective.""" ]
        ]
