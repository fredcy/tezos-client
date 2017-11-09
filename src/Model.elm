module Model exposing (Error(HttpError, OtherError), PageState(Loaded), Model, getPage)

import Date exposing (Date)
import Http
import Table
import Data.Schema as Schema
import Data.Chain as Chain
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

    -- TODO move the fields below into some state for the specific pages?
    , tableState : Table.State
    , query : String
    , transactionTableState : Table.State
    , contractTableState : Table.State
    , peerTableState : Table.State
    }


getPage : PageState -> Page
getPage (Loaded page) =
    page
