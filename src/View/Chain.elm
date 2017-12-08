module View.Chain exposing (view)

import Array
import Date exposing (Date)
import Date.Distance
import Html as H exposing (Html)
import Html.Attributes as HA
import InfiniteScroll
import ParseInt
import Sha256
import Window
import Data.Chain as Chain
import Route
import Msg exposing (Msg(InfiniteScroll))
import View.Field exposing (formatDate, shortHash)


view : Date -> Window.Size -> InfiniteScroll.Model Msg -> Chain.Model -> Html Msg
view now windowSize scrollState model =
    let
        blockSummaries =
            model.blockSummaries

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
            , footer scrollState
            ]


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
        , H.td
            [ HA.class "baker"
            , HA.title bs.baker
            , HA.style [ ( "color", bakerColor bs.baker ) ] |> Debug.log "style"
            ]
            [ H.text (shortHash bs.baker) ]
        ]


saturations =
    Array.fromList [ 35, 50, 65 ]


lightnesses =
    Array.fromList [ 35, 50, 65 ]


{-| Generate a CSS color value (as a string) from the id hash of a baker.
This follows the scheme of <https://github.com/zenozeng/color-hash>.
-}
bakerColor : String -> String
bakerColor bakerHash =
    let
        hashInt =
            String.dropLeft 3 bakerHash
                -- skipped common "tz1" prefix
                |> Sha256.sha256
                -- could have been any hash to hex
                |> String.left 14
                -- kept implicit numeric value in reasonable range
                |> ParseInt.parseIntHex
                -- should never fail, but it *is* a Result value so ...
                |> Result.withDefault 0

        hue =
            hashInt % 359

        hash2 =
            hashInt // 360

        saturation =
            Array.get (hash2 % Array.length saturations) saturations |> Maybe.withDefault 50

        hash3 =
            hash2 // Array.length saturations

        lightness =
            Array.get (hash3 % Array.length lightnesses) lightnesses |> Maybe.withDefault 50

        hsl =
            "hsl(" ++ toString hue ++ "," ++ toString saturation ++ "%," ++ toString lightness ++ "%)"
    in
        Debug.log "hsl" hsl
