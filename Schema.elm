module Schema exposing (..)

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode


type Msg
    = ClickField Context


type SchemaData
    = SchemaObject (Dict String ( Bool, SchemaData ))
    | SchemaList (List SchemaData)
    | SchemaString String
    | SchemaInt Int
    | SchemaBool Bool


type ContextItem
    = FieldName String
    | ListIndex Int


type alias Context =
    List ContextItem


decodeSchema : Decode.Decoder SchemaData
decodeSchema =
    Decode.oneOf
        [ Decode.keyValuePairs (Decode.lazy (\_ -> decodeSchema)) |> Decode.map makeObject
        , Decode.list (Decode.lazy (\_ -> decodeSchema)) |> Decode.map SchemaList
        , Decode.string |> Decode.map SchemaString
        , Decode.int |> Decode.map SchemaInt
        , Decode.bool |> Decode.map SchemaBool
        ]


makeObject : List ( String, SchemaData ) -> SchemaData
makeObject fields =
    let
        fieldsWithVisible =
            List.map (\( k, v ) -> ( k, ( True, v ) )) fields
    in
        SchemaObject (Dict.fromList fieldsWithVisible)


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
        SchemaObject properties ->
            viewSchemaObject context properties

        SchemaList items ->
            viewSchemaList context items

        SchemaString s ->
            H.text ("\"" ++ s ++ "\"")

        SchemaInt i ->
            H.text (toString i)

        SchemaBool b ->
            H.text (toString b)


viewSchemaObject : Context -> Dict String ( Bool, SchemaData ) -> Html Msg
viewSchemaObject context properties =
    let
        viewField ( name, ( visible, value ) ) =
            let
                newContext =
                    FieldName name :: context
            in
                H.div [ margin, HA.classList [ ( "collapsed", not visible ) ] ]
                    [ H.span [ HE.onClick (ClickField newContext) ] [ H.text (name ++ ": ") ]
                    , viewSchemaData newContext value
                    ]
    in
        H.div [ HA.class "object" ]
            (Dict.toList properties |> List.map viewField)


viewSchemaList context items =
    let
        viewItem i item =
            H.li [] [ viewSchemaData (ListIndex i :: context) item ]
    in
        H.ul [ margin ] (List.indexedMap viewItem items)


margin : H.Attribute Msg
margin =
    HA.style [ ( "margin-left", "15px" ) ]


update : Msg -> Maybe SchemaData -> Maybe SchemaData
update msg schemaDataMaybe =
    case msg of
        ClickField context ->
            Maybe.map (toggleVisible (List.reverse context)) schemaDataMaybe


{-| Change the visibility flag for a particular field in the schema. The
    `context` is a path of field names down from the root of the input schema.
-}
toggleVisible : Context -> SchemaData -> SchemaData
toggleVisible context schemaData =
    case context of
        [] ->
            schemaData |> Debug.log "no match for context"

        contextItem :: subContext ->
            case schemaData of
                SchemaObject fields ->
                    let
                        toggle : ( Bool, SchemaData ) -> ( Bool, SchemaData )
                        toggle ( visible, value ) =
                            if subContext == [] then
                                ( not visible, value )
                            else
                                ( visible, toggleVisible subContext value )

                        newFields : String -> Dict String ( Bool, SchemaData )
                        newFields fieldName =
                            Dict.update fieldName (Maybe.map toggle) fields
                    in
                        case contextItem of
                            FieldName fieldName ->
                                SchemaObject (newFields fieldName)

                            _ ->
                                schemaData |> schemaError ("no match for " ++ toString context)

                SchemaList items ->
                    let
                        toggle i contextIndex item =
                            if i == contextIndex then
                                toggleVisible subContext item
                            else
                                item
                    in
                        case contextItem of 
                            ListIndex contextIndex ->
                                SchemaList (List.indexedMap (toggle contextIndex) items)

                            _ ->
                                schemaData |> schemaError ("no match for " ++ toString context)

                _ ->
                    schemaData |> schemaError ("no match for " ++ toString context)


schemaError : String -> SchemaData -> SchemaData
schemaError msg data =
    let
        _ =
            Debug.log "schema error" msg
    in
        data
