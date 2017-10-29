module View.Field exposing (..)

import Date exposing (Date)
import Date.Extra.Format
import Date.Extra.Config.Config_en_us
import FormatNumber
import FormatNumber.Locales
import Data.Chain as Chain

formatCentiles : Int -> String
formatCentiles number =
    let
        usLocale =
            FormatNumber.Locales.usLocale
    in
        (toFloat number / 100)
            |> FormatNumber.format { usLocale | decimals = 2 }


formatDate : Date -> String
formatDate date =
    Date.Extra.Format.formatUtc Date.Extra.Config.Config_en_us.config "%Y-%m-%dT%H:%M:%SZ" date

shortHash : Chain.Base58CheckEncodedSHA256 -> String
shortHash hash =
    String.left 14 hash
