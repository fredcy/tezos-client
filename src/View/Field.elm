module View.Field exposing (..)

import FormatNumber
import FormatNumber.Locales


formatCentiles : Int -> String
formatCentiles number =
    let
        usLocale =
            FormatNumber.Locales.usLocale
    in
        (toFloat number / 100)
            |> FormatNumber.format { usLocale | decimals = 2 }
