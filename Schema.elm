module Schema exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode


type SchemaData
    = SchemaObject (List ( String, SchemaData ))
    | SchemaList (List SchemaData)
    | SchemaString String
    | SchemaInt Int
    | SchemaBool Bool


decodeSchema : Decode.Decoder SchemaData
decodeSchema =
    Decode.oneOf
        [ Decode.keyValuePairs (Decode.lazy (\_ -> decodeSchema)) |> Decode.map SchemaObject
        , Decode.list (Decode.lazy (\_ -> decodeSchema)) |> Decode.map SchemaList
        , Decode.string |> Decode.map SchemaString
        , Decode.int |> Decode.map SchemaInt
        , Decode.bool |> Decode.map SchemaBool
        ]


viewSchemaDataRaw : Maybe SchemaData -> Html msg
viewSchemaDataRaw schemaDataMaybe =
    case schemaDataMaybe of
        Just schemaData ->
            H.pre [ HA.style [ ( "white-space", "pre-wrap" ) ] ] [ H.text (toString schemaData) ]

        Nothing ->
            H.text "[no schema data]"


viewSchemaData : Int -> SchemaData -> Html msg
viewSchemaData depth schemaData =
    case schemaData of
        SchemaObject properties ->
            viewSchemaObject depth properties

        SchemaList items ->
            viewSchemaList depth items

        SchemaString s ->
            H.text ("\"" ++ s ++ "\"")

        SchemaInt i ->
            H.text (toString i)

        SchemaBool b ->
            H.text (toString b)


viewSchemaObject : Int -> List ( String, SchemaData ) -> Html msg
viewSchemaObject depth properties =
    let
        viewField ( name, value ) =
            H.div [ margin ]
                [ H.text (name ++ ": ")
                , viewSchemaData (depth + 1) value
                ]
    in
        H.div []
            (List.map viewField properties)


viewSchemaList depth items =
    let
        viewItem item =
            H.li [] [ viewSchemaData depth item ]
    in
        H.ul [ margin ] (List.map viewItem items)


margin : H.Attribute msg
margin =
    HA.style [ ( "margin-left", "15px" ) ]
