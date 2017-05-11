module View.Page exposing (frame)

import Date exposing (Date)
import Html as H exposing (Html)
import Html.Attributes as HA
import Model
import Route exposing (Route)


type alias Context =
    { pageState : Model.PageState
    , now : Date
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
        [ H.h1 [] [ H.text "Tezos client" ]
        , navLinks
        , H.div [] [ H.text (toString context) ]
        , H.hr [] []
        ]


links : List ( String, Route )
links =
    [ ( "Home", Route.Home )
    , ( "Heads", Route.Heads )
    , ( "Operations", Route.Operations )
    , ( "Schemas", Route.Schema )

    --, ( "Debug", Route.Debug )
    ]


navLinks : Html msg
navLinks =
    let
        makeNavLink ( label, route ) =
            H.li [] [ H.a [ Route.href route ] [ H.text label ] ]
    in
        H.ul [ HA.class "nav-links" ] (List.map makeNavLink links)
