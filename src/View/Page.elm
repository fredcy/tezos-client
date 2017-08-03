module View.Page exposing (frame)

import Date exposing (Date)
import Html as H exposing (Html)
import Html.Attributes as HA
import Date.Extra.Format
import Date.Extra.Config.Config_en_us
import Model
import Route exposing (Route)
import Update


type alias Context =
    { pageState : Model.PageState
    , now : Date
    , errorCount : Int
    }


frame : Context -> Html msg -> Html msg
frame context content =
    H.div [ HA.class "page-frame" ]
        [ viewHeader context
        , content

        --, viewFooter
        ]


viewHeader : Context -> Html msg
viewHeader context =
    H.div [ HA.class "page-header" ]
        [ H.h1 [] [ H.text "Tezos Explorer" ]
        , viewNow context.now
        , viewErrorCount context.errorCount
        , navLinks context.pageState
        ]


formatDate : String -> Date -> String
formatDate =
    Date.Extra.Format.format Date.Extra.Config.Config_en_us.config


viewNow : Date -> Html msg
viewNow now =
    H.div
        [ HA.class "now"
        , HA.title (formatDate "%Y/%m/%d %H:%M:%S" now)
        ]
        [ H.text (formatDate "%H:%M" now) ]


links : List ( String, Route )
links =
    [ ( "Home", Route.Home )
    , ( "Heads", Route.Heads )
    , ( "Operations", Route.Operations )
    , ( "Contracts", Route.Contracts )
    , ( "Keys", Route.Keys )
    , ( "Peers", Route.Peers )
    , ( "Schemas", Route.Schema )
    , ( "About", Route.About )

    --, ( "Debug", Route.Debug )
    ]


navLinks : Model.PageState -> Html msg
navLinks pageState =
    let
        makeNavLink ( label, route ) =
            H.li [ HA.classList [ ( "active", isRouteForPage route pageState ) ] ]
                [ H.a [ Route.href route ] [ H.text label ] ]
    in
        H.ul [ HA.class "nav-links" ] (List.map makeNavLink links)


isRouteForPage : Route -> Model.PageState -> Bool
isRouteForPage route (Model.Loaded page) =
    Update.toPage route == page


viewErrorCount : Int -> Html msg
viewErrorCount count =
    H.div
        [ HA.classList
            [ ( "error-count", True )
            , ( "have-errors", count > 0 )
            ]
        ]
        [ H.a [ Route.href Route.Errors ] [ H.text (toString count) ] ]
