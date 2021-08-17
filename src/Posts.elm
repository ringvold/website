module Posts exposing (..)

import DataSource.Glob as Glob
import DataSource exposing (DataSource)

all: DataSource (List String)
all =
   Glob.succeed identity
        |> Glob.match (Glob.literal "content/blog/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toDataSource