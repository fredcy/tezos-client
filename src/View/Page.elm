module View.Page exposing (frame)

import Html as H exposing (Html)
import Html.Attributes as HA
import Route exposing (Route)


frame : Html msg -> Html msg
frame content =
    H.div [ HA.class "page-frame" ]
        [ viewHeader
        , content
          --, viewFooter
        ]


viewHeader : Html msg
viewHeader =
    H.div [ HA.class "page-header" ]
        [ navLinks
        , H.hr [] []
        ]


links : List ( String, Route )
links =
    [ ( "Home", Route.Home )
    , ( "Operations", Route.Operations )
    , ( "Schemas", Route.Schema )
    , ( "Debug", Route.Debug )
    ]


navLinks : Html msg
navLinks =
    let
        makeNavLink ( label, route ) =
            H.li [] [ H.a [ Route.href route ] [ H.text label ] ]
    in
        H.ul [ HA.class "nav-links" ] (List.map makeNavLink links)
