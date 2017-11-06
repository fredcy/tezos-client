module Route exposing (Route(..), fromLocation, href, modifyUrl, newUrl)

import Html as H
import Html.Attributes as HA
import UrlParser as Url exposing (parseHash, s, (</>), Parser)
import Navigation exposing (Location)
import Data.Chain exposing (BlockID, OperationID, ContractID, AccountID)


type Route
    = Home
    | Block BlockID
    | Operations
    | Operation OperationID
    | ChainAt BlockID
    | Contracts
    | Keys
    | Peers
    | Contract ContractID
    | Schema
    | Heads
    | Errors
    | Debug
    | About
    | Chain2
    | Accounts
    | Account AccountID


route : Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home (s "")
        , Url.map Home (s "home")
        , Url.map Heads (s "heads")
        , Url.map Block (s "block" </> Url.string)
        , Url.map Operations (s "operations")
        , Url.map Operation (s "operation" </> Url.string)
        , Url.map ChainAt (s "chainat" </> Url.string)
        , Url.map Contracts (s "contracts")
        , Url.map Keys (s "keys")
        , Url.map Peers (s "peers")
        , Url.map Contract (s "contract" </> Url.string)
        , Url.map Schema (s "schema")
        , Url.map Debug (s "debug")
        , Url.map Errors (s "errors")
        , Url.map About (s "about")
        , Url.map Chain2 (s "chain2")
        , Url.map Accounts (s "accounts")
        , Url.map Account (s "account" </> Url.string)
        ]


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Home ->
                    []

                Block hash ->
                    [ "block", hash ]

                Operations ->
                    [ "operations" ]

                Operation operationId ->
                    [ "operation", operationId ]

                ChainAt hash ->
                    [ "chainat", hash ]

                Contracts ->
                    [ "contracts" ]

                Keys ->
                    [ "keys" ]

                Peers ->
                    [ "peers" ]

                Contract contractId ->
                    [ "contract", contractId ]

                Schema ->
                    [ "schema" ]

                Heads ->
                    [ "heads" ]

                Debug ->
                    [ "debug" ]

                Errors ->
                    [ "errors" ]

                About ->
                    [ "about" ]

                Chain2 ->
                    [ "chain2" ]

                Accounts ->
                    [ "accounts" ]

                Account accountId ->
                    [ "account", accountId ]
    in
        "#/" ++ String.join "/" pieces


href : Route -> H.Attribute msg
href route =
    HA.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


newUrl : Route -> Cmd msg
newUrl =
    routeToString >> Navigation.newUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
