module View.Chain exposing (view)

import Array
import Date exposing (Date)
import Date.Distance
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import InfiniteScroll
import ParseInt
import Sha256
import Window
import Data.Chain as Chain
import HashColor
import Route
import Msg exposing (Msg(InfiniteScroll))
import View.Field exposing (formatDate, shortHash)


view : Date -> Window.Size -> InfiniteScroll.Model Msg -> Chain.Model -> Html Msg
view now windowSize scrollState model =
    let
        thead =
            H.thead []
                [ H.tr []
                    [ H.th [] [ H.text "level" ]
                    , H.th [] [ H.text "hash" ]
                    , H.th [] [ H.text "timestamp" ]
                    , H.th [ HA.class "timestamp" ] [ H.text "age" ]
                    , H.th [] [ H.text "ops" ]
                    , H.th [] [ H.text "priority" ]
                    , H.th [] [ H.text "baker" ]
                    ]
                ]

        headerFooterAllowance =
            205

        heightValue =
            toString (windowSize.height - headerFooterAllowance) ++ "px"
    in
        H.div []
            -- This outer div allows us to exclude the footer from the scrolling area.
            [ H.div
                [ HA.class "blockchain-container"
                , HA.style [ ( "height", heightValue ), ( "overflow", "scroll" ) ]
                , InfiniteScroll.infiniteScroll Msg.InfiniteScroll
                ]
                [ H.div
                    -- This inner div is a kludge forcing the content to always
                    -- overflow the wrapping div, hence forcing a scrollbar,
                    -- hence allowing elm-infinite-scroll to work even if the
                    -- content doesn't fill the div. [I don't see the scroll bar
                    -- on an iPad.]
                    [ HA.style [ ( "min-height", "101%" ) ] ]
                    [ H.table [ HA.class "blockchain" ]
                        [ thead
                        , H.tbody [] (blockRows now model.blockSummaries)
                        ]
                    ]
                ]
            , footer scrollState
            ]


{-| blockRows maps the list of ChainSummary values into HTML table rows. We use
List.foldr rather than a simple List.map so that we can memoize the calculation
of CSS color from baker id hash. [premature optimization?] This memoization
seems very clumsy but I don't see any other way to thread the color-dictionary
to where it is needed.
-}
blockRows : Date -> List Chain.BlockSummary -> List (Html Msg)
blockRows now blockSummaries =
    let
        reduce : Chain.BlockSummary -> ( HashColor.Model, List (Html Msg) ) -> ( HashColor.Model, List (Html Msg) )
        reduce bs ( colorDict, rows ) =
            let
                ( colorDict2, row ) =
                    viewBlockSummary now colorDict bs
            in
                ( colorDict2, row :: rows )
    in
        List.foldr reduce ( Dict.empty, [] ) blockSummaries
            |> Tuple.second


footer : InfiniteScroll.Model msg -> Html msg
footer scrollState =
    let
        msg =
            if InfiniteScroll.isLoading scrollState then
                "loading ..."
            else
                "scroll for more"
    in
        H.div [ HA.class "scroll-footer" ] [ H.text msg ]


viewBlockSummary : Date -> HashColor.Model -> Chain.BlockSummary -> ( HashColor.Model, Html Msg )
viewBlockSummary now colorDict bs =
    let
        ( color, colorDict2 ) =
            String.dropLeft 3 bs.baker |> HashColor.toColorMemo colorDict

        row =
            H.tr [ HA.class "block" ]
                [ H.td [] [ H.text (toString bs.level) ]
                , H.td
                    [ HA.class "hash"
                    , HA.title bs.hash
                    ]
                    [ H.a [ Route.href (Route.Block bs.hash) ] [ H.text (shortHash bs.hash) ] ]
                , H.td [ HA.class "timestamp" ]
                    [ H.text (formatDate bs.timestamp) ]
                , H.td [ HA.class "age" ]
                    [ H.text (Date.Distance.inWords now bs.timestamp) ]
                , H.td [ HA.class "operation-count" ]
                    [ H.text (toString bs.opCount) ]
                , H.td [ HA.class "priority number" ]
                    [ H.text (toString bs.priority) ]
                , H.td
                    [ HA.class "baker"
                    , HA.title bs.baker
                    , HA.style [ ( "color", color ) ]
                    ]
                    [ H.text (shortHash bs.baker) ]
                ]
    in
        ( colorDict2, row )
