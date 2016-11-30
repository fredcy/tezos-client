module Schema exposing (..)

import Char
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode


type Msg
    = ClickField Context


type alias Visibility =
    Bool


type SchemaData
    = SchemaObject (Dict String ( Visibility, SchemaData ))
    | SchemaList (List ( Visibility, SchemaData ))
    | SchemaString String
    | SchemaInt Int
    | SchemaBool Bool
    | SchemaNull


type ContextItem
    = FieldName String
    | ListIndex Int


type alias Context =
    List ContextItem


decodeSchema : Decode.Decoder SchemaData
decodeSchema =
    Decode.oneOf
        [ Decode.keyValuePairs (Decode.lazy (\_ -> decodeSchema)) |> Decode.map makeObject
        , Decode.list (Decode.lazy (\_ -> decodeSchema)) |> Decode.map makeList
        , Decode.string |> Decode.map SchemaString
        , Decode.int |> Decode.map SchemaInt
        , Decode.bool |> Decode.map SchemaBool
        , Decode.null SchemaNull
        ]


makeList : List SchemaData -> SchemaData
makeList items =
    SchemaList (List.map (\item -> ( True, item )) items)


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


viewSchemaDataTop : SchemaData -> Html Msg
viewSchemaDataTop data =
    H.div [ HA.class "schemadata" ]
        [ H.h2 [] [ H.text "Raw Schema" ]
        , H.p [] [ H.text "Click labels and bullets to collapse and expand" ]
        , viewSchemaData [] data
        ]


viewSchemaData : Context -> SchemaData -> Html Msg
viewSchemaData context schemaData =
    case schemaData of
        SchemaObject properties ->
            viewSchemaObject context properties

        SchemaList items ->
            viewSchemaList context items

        SchemaString s ->
            H.span [ HA.class "string" ] [ H.text ("\"" ++ s ++ "\"") ]

        SchemaInt i ->
            H.span [ HA.class "int" ] [ H.text (toString i) ]

        SchemaBool b ->
            H.span [ HA.class "bool" ] [ H.text (toString b) ]

        SchemaNull ->
            H.span [ HA.class "null" ] [ H.text "null" ]


viewSchemaObject : Context -> Dict String ( Bool, SchemaData ) -> Html Msg
viewSchemaObject context properties =
    let
        viewField ( name, ( visible, value ) ) =
            let
                newContext =
                    FieldName name :: context
            in
                H.div [ margin, HA.classList [ ( "collapsed", not visible ) ] ]
                    [ H.span [ HA.class "fieldlabel", HE.onClick (ClickField newContext) ] [ H.text (name ++ ": ") ]
                    , viewSchemaData newContext value
                    ]
    in
        H.div [ HA.class "object" ]
            (Dict.toList properties |> List.map viewField)


listGlyph =
    let
        bulletString =
            Char.fromCode 9679 |> String.fromChar
    in
        H.span [ HA.class "bullet" ] [ H.text bulletString ]


viewSchemaList context items =
    let
        viewItem i ( visible, item ) =
            let
                newContext =
                    ListIndex i :: context
            in
                H.li [ HA.classList [ ( "collapsed", not visible ) ] ]
                    [ H.div [ HA.class "listmark", HE.onClick (ClickField newContext) ] [ listGlyph ]
                    , viewSchemaData (ListIndex i :: context) item
                    ]
    in
        H.ul [ margin, HA.class "list" ] (List.indexedMap viewItem items)


margin : H.Attribute Msg
margin =
    HA.style [ ( "margin-left", "1.5em" ) ]


update : Msg -> Maybe SchemaData -> Maybe SchemaData
update msg schemaDataMaybe =
    case msg of
        ClickField context ->
            Maybe.map (toggleVisible (List.reverse context)) schemaDataMaybe


{-| Change the visibility flag for a particular field in the schema. The
    `context` is a path of field names and list indexes down from the root of
    the input schema.
-}
toggleVisible : Context -> SchemaData -> SchemaData
toggleVisible context schemaData =
    case context of
        [] ->
            schemaData |> schemaError "ran out of context"

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
                        toggle i contextIndex ( visible, subSchema ) =
                            if i == contextIndex then
                                if subContext == [] then
                                    ( not visible, subSchema )
                                else
                                    ( visible, toggleVisible subContext subSchema )
                            else
                                ( visible, subSchema )
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
            Debug.log "schema error:" msg
    in
        data


mapSchemaData : (( Visibility, SchemaData ) -> ( Visibility, SchemaData )) -> SchemaData -> SchemaData
mapSchemaData fn schemaData =
    case schemaData of
        SchemaObject items ->
            let
                mapFn _ v =
                    fn v
            in
                SchemaObject (Dict.map (\_ v -> fn v) items)

        -- TODO: continue here
        _ ->
            schemaData
