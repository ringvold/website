module Posts exposing (..)

import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import OptimizedDecoder as Decode exposing (Decoder)


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


draftDecoder : String -> DataSource Bool
draftDecoder filePath =
    DataSource.File.onlyFrontmatter
        (Decode.optionalField "draft" Decode.bool)
        filePath
        |> DataSource.map (Maybe.withDefault False)
