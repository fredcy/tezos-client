module Route exposing (Route(..), fromLocation, href, modifyUrl, newUrl)

import Html as H
import Html.Attributes as HA
import UrlParser as Url exposing (parseHash, s, (</>), string, oneOf, Parser)
import Navigation exposing (Location)
import Data.Chain as Chain exposing (BlockID, OperationID)


type Route
    = Home
    | Block BlockID
    | Operations
    | Operation OperationID
    | ChainAt BlockID
    | Contracts
    | Schema
    | Heads
    | Errors
    | Debug


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
        , Url.map Schema (s "schema")
        , Url.map Debug (s "debug")
        , Url.map Errors (s "errors")
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

                Schema ->
                    [ "schema" ]

                Heads ->
                    [ "heads" ]

                Debug ->
                    [ "debug" ]

                Errors ->
                    [ "errors" ]
    in
        "#/" ++ (String.join "/" pieces)


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
