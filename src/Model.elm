module Model exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Data.Schema as Schema
import Data.Chain as Chain exposing (BlockID, OperationID)
import Page exposing (Page)


{-| This page state comes from elm-spa-example.
    TODO: Make full use of transition state, or remove.
-}
type PageState
    = Loaded Page


type alias Model =
    { schemaData : Schema.Model
    , errors : List Http.Error
    , nodeUrl : String
    , showBlock : Maybe BlockID
    , showOperation : Maybe OperationID
    , showBranch : Maybe BlockID
    , now : Date
    , chain : Chain.Model
    , pageState : PageState
    }


getPage : PageState -> Page
getPage (Loaded page) =
    page
