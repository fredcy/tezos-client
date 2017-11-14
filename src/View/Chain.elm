module View.Chain exposing (view)

import Date exposing (Date)
import Date.Distance
import Html as H exposing (Html)
import Html.Attributes as HA
import InfiniteScroll
import Window
import Data.Chain as Chain
import Route
import Msg exposing (Msg(InfiniteScroll))
import View.Field exposing (formatDate, shortHash)


view : Date -> Window.Size -> Chain.Model -> Html Msg
view now windowSize model =
    viewChainSummary now windowSize model.blockSummaries


viewChainSummary : Date -> Window.Size -> List Chain.BlockSummary -> Html Msg
viewChainSummary now windowSize blockSummaries =
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
            200

        heightValue =
            toString (windowSize.height - headerFooterAllowance) ++ "px"
    in
        H.div
            [ HA.class "blockchain-container"
            , HA.style [ ( "height", heightValue ), ( "overflow", "scroll" ) ]
            , InfiniteScroll.infiniteScroll Msg.InfiniteScroll
            ]
            [ H.div
                -- This inner div is a kludge forcing the content to always
                -- overflow the wrappig div, hence forcing a scrollbar, hence
                -- allowing elm-infinite-scroll to work even if the content
                -- doesn't fill the div.
                [ HA.style [ ( "min-height", "101%" ) ] ]
                [ H.table [ HA.class "blockchain" ]
                    [ thead
                    , H.tbody [] (List.map (viewBlockSummary now) blockSummaries)
                    ]
                ]
            ]


viewBlockSummary : Date -> Chain.BlockSummary -> Html Msg
viewBlockSummary now bs =
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
        , H.td [ HA.class "baker", HA.title bs.baker ]
            [ H.text (shortHash bs.baker) ]
        ]
