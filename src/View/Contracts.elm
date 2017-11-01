module View.Contracts exposing (..)

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import RemoteData exposing (RemoteData)
import Table
import Tuple exposing (first, second)
import Data.Chain as Chain exposing (AccountSummary, ContractID, Contract)
import Data.Michelson as Michelson
import Update exposing (Msg, Msg(SetTableState, SetQuery, SetContractTableState))
import View.Field as VF exposing (formatDate, shortHash, formatCentiles)
import Route


type alias ContractInfo =
    ( ContractID, RemoteData Http.Error Contract )


config : Table.Config ContractInfo Msg
config =
    Table.config
        { toId = first
        , toMsg = SetContractTableState
        , columns =
            [ contractHashColumn "contract" first
            , tezColumn "balance (ꜩ)" (second >> RemoteData.map .balance >> RemoteData.toMaybe)
            , contractHashColumn "manager" (second >> RemoteData.map .manager >> RemoteData.withDefault "")
            , intColumn "counter" (second >> RemoteData.map .counter >> RemoteData.toMaybe)
            , intColumn "script size" (second >> RemoteData.map (.script >> scriptSize) >> RemoteData.withDefault Nothing)
            ]
        }


contractHashColumn : String -> (ContractInfo -> String) -> Table.Column ContractInfo msg
contractHashColumn name toHash =
    let
        hashDetails hash =
            Table.HtmlDetails [ HA.class "hash" ]
                [ H.a [ Route.href (Route.Contract hash) ] [ H.text (shortHash hash) ] ]
    in
        Table.veryCustomColumn
            { name = name
            , viewData = \data -> hashDetails (toHash data)
            , sorter = Table.increasingOrDecreasingBy toHash
            }


tezColumn : String -> (ContractInfo -> Maybe Int) -> Table.Column ContractInfo msg
tezColumn name toTezMaybe =
    Table.customColumn
        { name = name
        , viewData = toTezMaybe >> Maybe.map formatCentiles >> Maybe.withDefault "..."
        , sorter = Table.increasingOrDecreasingBy (toTezMaybe >> Maybe.withDefault 0)
        }


intColumn : String -> (a -> Maybe Int) -> Table.Column a msg
intColumn name toInt =
    Table.customColumn
        { name = name
        , viewData = toInt >> Maybe.map toString >> Maybe.withDefault ""
        , sorter = Table.increasingOrDecreasingBy (toInt >> Maybe.withDefault 0)
        }


scriptSize : Maybe Michelson.Script -> Maybe Int
scriptSize scriptMaybe =
    scriptMaybe |> Maybe.andThen (toString >> String.length >> nonZero)


nonZero : Int -> Maybe Int
nonZero i =
    if i == 0 then
        Nothing
    else
        Just i


view : List ContractID -> Dict ContractID (RemoteData Http.Error Contract) -> Table.State -> Html Msg
view contractIDs contractDataDict tableState =
    let
        contractData : List ( ContractID, RemoteData Http.Error Contract )
        contractData =
            contractIDs
                |> List.map (\id -> ( id, Dict.get id contractDataDict |> Maybe.withDefault RemoteData.Loading ))
    in
        H.div [ HA.class "contracts" ]
            [ Table.view config tableState contractData ]



{-
   viewContractTable : List ContractID -> Dict ContractID (RemoteData Http.Error Contract) -> Html Msg
   viewContractTable contractIDs contracts =
       let
           thead =
               H.tr []
                   [ H.th [] [ H.text "contractID" ]
                   , H.th [] [ H.text "balance (ꜩ)" ]
                   , H.th [] [ H.text "manager" ]
                   , H.th [] [ H.text "counter" ]
                   , H.th [] [ H.text "script size" ]
                   ]

           trow contractId =
               let
                   contractData =
                       Dict.get contractId contracts
                           |> Maybe.withDefault RemoteData.NotAsked
               in
                   case contractData of
                       RemoteData.Success contract ->
                           H.tr []
                               [ H.td [ HA.class "hash" ]
                                   [ H.a [ Route.href (Route.Contract contractId) ]
                                       [ H.text (shortHash contractId) ]
                                   ]
                               , H.td [ HA.class "balance" ] [ H.text (formatCentiles contract.balance) ]
                               , H.td [ HA.class "hash" ]
                                   [ H.a [ Route.href (Route.Contract contract.manager) ]
                                       [ H.text (shortHash contract.manager) ]
                                   ]
                               , H.td [ HA.class "number" ] [ H.text (toString contract.counter) ]
                               , H.td [ HA.class "number" ]
                                   [ contract.script
                                       |> Maybe.map (toString >> String.length >> toString)
                                       |> Maybe.withDefault ""
                                       |> H.text
                                   ]
                               ]

                       RemoteData.NotAsked ->
                           H.tr []
                               [ H.td [ HA.class "hash link" ] [ H.text (shortHash contractId) ]
                               , H.td [] [ H.text "..." ]
                               ]

                       RemoteData.Loading ->
                           H.tr []
                               [ H.td [ HA.class "hash link" ] [ H.text (shortHash contractId) ]
                               , H.td [] [ H.text "......" ]
                               ]

                       RemoteData.Failure error ->
                           H.tr []
                               [ H.td [ HA.class "hash link" ] [ H.text (shortHash contractId) ]
                               , H.td [] [ H.text (toString error) ]
                               ]

           tbody =
               H.tbody [] (List.map trow (List.sort contractIDs))
       in
           H.table [ HA.class "contracts" ] [ thead, tbody ]
-}
