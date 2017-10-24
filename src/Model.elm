module Model exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Table
import Data.Schema as Schema
import Data.Chain as Chain exposing (BlockID, OperationID)
import Page exposing (Page)


{-| This page state comes from elm-spa-example.
TODO: Make full use of transition state, or remove.
-}
type PageState
    = Loaded Page


type Error
    = HttpError Http.Error
    | OtherError String


type alias Model =
    { schemaData : Schema.Model
    , errors : List Error
    , nodeUrl : String
    , now : Date
    , chain : Chain.Model
    , pageState : PageState

    -- TODO move the two fields below into some state for Accounts page.
    , tableState : Table.State
    , query : String
    }


getPage : PageState -> Page
getPage (Loaded page) =
    page
