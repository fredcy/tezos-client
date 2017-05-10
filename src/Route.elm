module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Html as H
import Html.Attributes as HA
import UrlParser as Url exposing (parseHash, s, (</>), string, oneOf, Parser)
import Navigation exposing (Location)


type Route
    = Home
    | Operations
    | Schema
    | Debug


route : Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home (s "")
        , Url.map Home (s "home")
        , Url.map Operations (s "operations")
        , Url.map Schema (s "schema")
        , Url.map Debug (s "debug")
        ]


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Home ->
                    []

                Operations ->
                    [ "operations" ]

                Schema ->
                    [ "schema" ]

                Debug ->
                    [ "debug" ]
    in
        "#/" ++ (String.join "/" pieces)


href : Route -> H.Attribute msg
href route =
    HA.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
