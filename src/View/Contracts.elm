module View.Contracts exposing (view)

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Http
import RemoteData exposing (RemoteData)
import Table
import Data.Chain exposing (ContractID, Contract)
import Data.Michelson as Michelson
import Msg exposing (Msg(SetContractTableState))
import View.Field exposing (shortHash, formatCentiles)
import Route


type alias ContractView =
    { hash : ContractID
    , balance : Maybe Int
    , manager : ContractID
    , counter : Maybe Int
    , size : Maybe Int
    }


{-| View the sortable table of Contract summaries. We convert the model data
into ContractView format as as first pass to speed up sorting. Otherwise the
various RemoteData functions are called repeatedly in an implicit List.sortBy
-}
view : List ContractID -> Dict ContractID (RemoteData Http.Error Contract) -> Table.State -> Html Msg
view contractIDs contractDataDict tableState =
    let
        makeView : ContractID -> ContractView
        makeView contractId =
            Dict.get contractId contractDataDict
                |> Maybe.map
                    (\d ->
                        { hash = contractId
                        , balance = RemoteData.map .balance d |> RemoteData.toMaybe
                        , manager = RemoteData.map .manager d |> RemoteData.withDefault ""
                        , counter = RemoteData.map .counter d |> RemoteData.toMaybe
                        , size = RemoteData.map (.script >> scriptSize) d |> RemoteData.withDefault Nothing
                        }
                    )
                |> Maybe.withDefault
                    { hash = contractId
                    , balance = Nothing
                    , manager = ""
                    , counter = Nothing
                    , size = Nothing
                    }

        contractViews =
            List.map makeView contractIDs
    in
        H.div [ HA.class "contracts" ]
            [ Table.view config tableState contractViews ]


config : Table.Config ContractView Msg
config =
    Table.config
        { toId = .hash
        , toMsg = SetContractTableState
        , columns =
            [ contractHashColumn "contract" .hash
            , tezColumn "balance (ꜩ)" .balance
            , contractHashColumn "manager" .manager
            , intColumn "counter" .counter
            , intColumn "script size" .size
            ]
        }


contractHashColumn : String -> (ContractView -> String) -> Table.Column ContractView msg
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


tezColumn : String -> (ContractView -> Maybe Int) -> Table.Column ContractView msg
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
