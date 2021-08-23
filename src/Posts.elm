module Posts exposing (..)

import DataSource exposing (DataSource)
import DataSource.Glob as Glob


all : DataSource (List String)
all =
    Glob.succeed identity
        |> Glob.match (Glob.literal "content/blog/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toDataSource


type alias Post =
    { slug : String
    , filePath : String
    }


all2 : DataSource (List Post)
all2 =
    Glob.succeed Post
        |> Glob.match (Glob.literal "content/blog/")
        |> Glob.capture Glob.wildcard
        |> Glob.match
            (Glob.oneOf
                ( ( "", () )
                , [ ( "/index", () ) ]
                )
            )
        |> Glob.match (Glob.literal ".md")
        |> Glob.captureFilePath
        |> Glob.toDataSource
