module Schema exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode


type Msg
    = ClickField Context


type SchemaData
    = SchemaObject Bool (List ( String, SchemaData ))
    | SchemaList (List SchemaData)
    | SchemaString String
    | SchemaInt Int
    | SchemaBool Bool


type alias Context =
    List String


decodeSchema : Decode.Decoder SchemaData
decodeSchema =
    Decode.oneOf
        [ Decode.keyValuePairs (Decode.lazy (\_ -> decodeSchema)) |> Decode.map (SchemaObject True)
        , Decode.list (Decode.lazy (\_ -> decodeSchema)) |> Decode.map SchemaList
        , Decode.string |> Decode.map SchemaString
        , Decode.int |> Decode.map SchemaInt
        , Decode.bool |> Decode.map SchemaBool
        ]


viewSchemaDataRaw : Maybe SchemaData -> Html Msg
viewSchemaDataRaw schemaDataMaybe =
    case schemaDataMaybe of
        Just schemaData ->
            H.pre [ HA.style [ ( "white-space", "pre-wrap" ) ] ] [ H.text (toString schemaData) ]

        Nothing ->
            H.text "[no schema data]"


viewSchemaData : Context -> SchemaData -> Html Msg
viewSchemaData context schemaData =
    case schemaData of
        SchemaObject visible properties ->
            viewSchemaObject context properties

        SchemaList items ->
            viewSchemaList context items

        SchemaString s ->
            H.text ("\"" ++ s ++ "\"")

        SchemaInt i ->
            H.text (toString i)

        SchemaBool b ->
            H.text (toString b)


viewSchemaObject : Context -> List ( String, SchemaData ) -> Html Msg
viewSchemaObject context properties =
    let
        viewField ( name, value ) =
            let
                newContext =
                    name :: context
            in
                H.div [ margin ]
                    [ H.span [ HE.onClick (ClickField newContext) ] [ H.text (name ++ ": ") ]
                    , viewSchemaData newContext value
                    ]
    in
        H.div []
            (List.map viewField properties)


viewSchemaList context items =
    let
        viewItem item =
            H.li [] [ viewSchemaData context item ]
    in
        H.ul [ margin ] (List.map viewItem items)


margin : H.Attribute Msg
margin =
    HA.style [ ( "margin-left", "15px" ) ]


update : Msg -> Maybe SchemaData -> Maybe SchemaData
update msg schemaDataMaybe =
    case msg of
        ClickField context ->
            Maybe.map (toggleVisible context) schemaDataMaybe


toggleVisible : Context -> SchemaData -> SchemaData
toggleVisible context schemaData =
    case context |> Debug.log "context" of
        [] ->
            schemaData |> Debug.log "no match for context"

        fieldName :: subContext ->
            case schemaData of
                SchemaObject visible fields ->
                    schemaData

                _ ->
                    schemaData |> Debug.log "expected object context"



