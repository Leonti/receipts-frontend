module FormatUtils exposing (formatTimestamp, formatMoney)

import Date exposing (Date)
import Date.Extra.Format as DateFormat
import Date.Extra.Config.Config_en_au exposing (config)


formatTimestamp : Int -> String
formatTimestamp timeStamp =
    (DateFormat.format config "%Y-%m-%d %H:%M") <| Date.fromTime (toFloat timeStamp)


formatMoney : Float -> String
formatMoney amount =
    "$" ++ (toString amount)
