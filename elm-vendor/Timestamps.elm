module Timestamps exposing (Timestamps, data, format, toIsoString)

import DataSource exposing (DataSource)
import DataSource.Port
import DateFormat
import Json.Encode
import List.Extra
import OptimizedDecoder as Decode exposing (Decoder)
import Result.Extra
import Time exposing (utc)


type alias Timestamps =
    { updated : Time.Posix
    , created : Time.Posix
    }


data : String -> DataSource Timestamps
data filePath =
    DataSource.Port.get "gitTimestamps"
        (Json.Encode.string filePath)
        (Decode.string
            |> Decode.map (String.trim >> String.split "\n")
            |> Decode.map (List.map secondsStringToPosix)
            |> Decode.map Result.Extra.combine
            |> Decode.map
                (Result.withDefault
                    [ Time.millisToPosix 0
                    , Time.millisToPosix 0
                    ]
                )
            |> Decode.map (firstAndLast Timestamps >> Result.fromMaybe "Error")
            |> Decode.andThen Decode.fromResult
        )


firstAndLast : (a -> a -> b) -> List a -> Maybe b
firstAndLast constructor list =
    Maybe.map2 constructor
        (List.head list)
        (List.Extra.last list)


secondsStringToPosix : String -> Result String Time.Posix
secondsStringToPosix posixTime =
    posixTime
        |> String.trim
        |> String.toInt
        |> Maybe.map (\unixTimeInSeconds -> (unixTimeInSeconds * 1000) |> Time.millisToPosix)
        |> Result.fromMaybe "Expected int"


format : Time.Posix -> String
format posix =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthNumber
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]
        utc
        posix


toIsoString : Time.Posix -> String
toIsoString posix =
    DateFormat.format
        [ DateFormat.yearNumber
        , DateFormat.text " "
        , DateFormat.monthFixed
        , DateFormat.text ", "
        , DateFormat.dayOfMonthFixed
        ]
        utc
        posix
