module View.Delegations exposing (view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import RemoteData
import Table
import Data.Chain exposing (DelegationSummary)
import HashColor
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
            , Table.view (config delegationsFiltered) tableState delegationsFiltered
            , viewFooter
            ]


canonIdHash : String -> String
canonIdHash =
    String.dropLeft 3


config : List DelegationSummary -> Table.Config DelegationSummary Msg
config delegations =
    let
        reduce hash model =
            HashColor.toColorMemo model hash |> Tuple.first

        colorHashPreload : HashColor.Model
        colorHashPreload =
            (List.map (.source >> canonIdHash) delegations
                ++ List.map (.delegate >> canonIdHash) delegations
            )
                |> List.foldl reduce HashColor.init
    in
        Table.config
            { toId = .source
            , toMsg = SetDelegationsTableState
            , columns =
                [ idHashColumn "Account" colorHashPreload .source
                , idHashColumn "Delegate" colorHashPreload .delegate
                , Table.stringColumn "Timestamp" (.timestamp >> VF.formatDate)
                , blockHashColumn "Block" .blockHash
                ]
            }


idHashColumn : String -> HashColor.Model -> (DelegationSummary -> String) -> Table.Column DelegationSummary msg
idHashColumn name hashColorModel toHash =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewIdHash hashColorModel (toHash data)
        , sorter = Table.increasingOrDecreasingBy toHash
        }


viewIdHash : HashColor.Model -> String -> Table.HtmlDetails msg
viewIdHash hashColorModel hash =
    Table.HtmlDetails
        [ HA.class "hash"
        , HA.style
            [ ( "color"
              , HashColor.toColorMemo hashColorModel (canonIdHash hash)
                    |> Tuple.second
              )
            ]
        ]
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
