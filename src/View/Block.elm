module View.Block exposing (viewOperationGroups)

import Dict
import Html as H exposing (Html)
import Html.Attributes as HA
import Api
import Data.Chain exposing (BlockID)
import Model exposing (Model)
import Route
import Update exposing (Msg)
import View.Field exposing (shortHash, formatCentiles)


viewOperationGroups : Model -> BlockID -> Html Msg
viewOperationGroups model blockHash =
    let
        operationGroupsMaybe =
            Dict.get blockHash model.chain.blockOperationGroups
    in
        H.div [ HA.class "operation-groups" ]
            [ H.h4 [] [ H.text "Operation Groups" ]
            , case operationGroupsMaybe of
                Nothing ->
                    H.text "..."

                Just operationGroups ->
                    H.table []
                        [ H.thead []
                            [ H.tr []
                                [ H.th [] [ H.text "hash" ]
                                , H.th [] [ H.text "source" ]
                                , H.th [] [ H.text "operations" ]
                                ]
                            ]
                        , H.tbody []
                            (List.map viewOperationGroup operationGroups)
                        ]
            ]


viewOperationGroup : Api.OperationGroup -> Html Msg
viewOperationGroup operationGroup =
    H.tr [ HA.class "operation-group" ]
        -- TODO Enable links to operation and account detail pages?
        [ H.td [ HA.class "hash group-hash" ] [ H.text (shortHash operationGroup.hash) ]
        , H.td [ HA.class "hash" ] [ H.text (shortHash operationGroup.source) ]
        , H.td [] [ viewOperations operationGroup.operations ]
        ]


viewOperations : List Api.Operation -> Html Msg
viewOperations operations =
    let
        row operation =
            H.tr [ HA.class "operation" ] [ H.td [] [ viewOperation operation ] ]
    in
        H.table [ HA.class "operations" ]
            [ H.tbody [] (List.map row operations) ]


viewOperation : Api.Operation -> Html Msg
viewOperation operation =
    case operation of
        Api.Endorsement { block, slot } ->
            H.span []
                [ H.text "Endorsement of "
                , H.a [ Route.href (Route.Block block), HA.title block, HA.class "hash" ]
                    [ H.text (shortHash block) ]
                , H.text (", slot " ++ toString slot)
                ]

        Api.SeedNonceRevelation { level, nonce } ->
            H.span []
                [ H.text ("SeedNonceRevelation at level " ++ toString level ++ " with nonce ")
                , H.span [ HA.class "hash nonce" ] [ H.text nonce ]
                ]

        Api.Transaction { amount, destination } ->
            H.span []
                [ H.text "Transaction of "
                , H.text (formatCentiles amount)
                , H.text " ꜩ to "
                , H.a [ Route.href (Route.Account destination), HA.title destination, HA.class "hash" ]
                    [ H.text (shortHash destination) ]
                ]

        Api.Faucet { id, nonce } ->
            H.span []
                [ H.text "Faucet "

                -- TODO are these valid as accounts? i.e, should I continue to link the id value?
                , H.a [ Route.href (Route.Account id), HA.title id, HA.class "hash" ]
                    [ H.text (shortHash id) ]
                , H.text " with nonce "
                , H.span [ HA.class "hash nonce" ] [ H.text nonce ]
                ]

        Api.Delegation delegate ->
            H.span []
                [ H.text "Delegation to "
                , H.a [ Route.href (Route.Account delegate), HA.title delegate, HA.class "hash" ]
                    [ H.text (shortHash delegate) ]
                ]

        _ ->
            H.text (toString operation)
