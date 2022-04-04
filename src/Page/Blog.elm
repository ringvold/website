module Page.Blog exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File
import Head
import Head.Seo as Seo
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Link
import MarkdownCodec
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Posts
import Route exposing (Route)
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw
import Timestamps exposing (Timestamps)
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


type alias Data =
    List BlogEntry


type alias BlogEntry =
    { info : { title : String, description : String }
    , draft : Bool
    , route : Route
    , timestamps : Timestamps
    }


posts : DataSource (List String)
posts =
    Posts.all


data : DataSource Data
data =
    Posts.all2
        |> DataSource.andThen
            (List.map
                (\{ slug, filePath } ->
                    DataSource.map4 BlogEntry
                        (MarkdownCodec.titleAndDescription filePath)
                        (draftDecoder filePath)
                        (DataSource.succeed <| Route.Blog__Slug_ { slug = slug })
                        (Timestamps.data filePath)
                )
                >> DataSource.combine
            )
        |> DataSource.map (List.filter (.draft >> not))


draftDecoder : String -> DataSource Bool
draftDecoder filePath =
    DataSource.File.onlyFrontmatter
        (Decode.optionalField "draft" Decode.bool)
        filePath
        |> DataSource.map (Maybe.withDefault False)


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = "Blog - ringvold.io"
    , body =
        List.map viewBlogEntry static.data
    }


viewBlogEntry : BlogEntry -> Html msg
viewBlogEntry entry =
    div []
        [ Html.h2
            [ css
                [ Breakpoints.md [ Tw.text_4xl ]
                , Tw.font_bold
                , Tw.font_sans
                , Tw.break_normal
                , Tw.text_gray_900
                , Tw.pt_6
                , Tw.pb_2
                , Tw.text_3xl
                ]
            ]
            [ Link.link entry.route [] (text entry.info.title) ]
        , Shared.timestampView entry
        , p [] [ text entry.info.description ]
        ]
