module Page.Blog.Slug_ exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import MarkdownCodec
import MarkdownHtmlRenderer
import MarkdownHtmlRenderer2
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Posts
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
    { slug : String }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , routes = routes
        , data = data
        }
        |> Page.buildNoState { view = view }


routes : DataSource (List RouteParams)
routes =
    Posts.all
        |> DataSource.map (List.map RouteParams)


findFileBySlug : RouteParams -> DataSource String
findFileBySlug routeParams =
    Glob.succeed identity
        |> Glob.match (Glob.literal "content/blog/")
        |> Glob.match (Glob.literal routeParams.slug)
        |> Glob.match
            (Glob.oneOf
                ( ( "", () )
                , [ ( "/index", () ) ]
                )
            )
        |> Glob.match (Glob.literal ".md")
        |> Glob.captureFilePath
        |> Glob.expectUniqueMatch


data : RouteParams -> DataSource Data
data routeParams =
    findFileBySlug routeParams
        |> DataSource.andThen
            (\filePath ->
                DataSource.map4 Data
                    (MarkdownCodec.titleAndDescription filePath)
                    (DataSource.succeed routeParams.slug)
                    (MarkdownCodec.withoutFrontmatter MarkdownHtmlRenderer2.renderer
                        filePath
                        |> DataSource.resolve
                    )
                    (Timestamps.data filePath)
            )


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Harald Ringvold"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "Post image"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.info.description
        , locale = Nothing
        , title = static.data.info.title
        }
        |> Seo.website


type alias Data =
    { info : { title : String, description : String }
    , slug : String
    , body : List (Html Msg)
    , timestamps : Timestamps
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = static.data.info.title ++ " - ringvold.io"
    , body =
        [ Html.h1
            [ css
                [ Tw.font_bold
                , Tw.font_sans
                , Tw.break_normal
                , Tw.text_gray_900
                , Tw.pt_6
                , Tw.pb_2
                , Breakpoints.md [ Tw.text_4xl ]
                , Tw.text_3xl
                ]
            ]
            [ Html.text static.data.info.title ]
        ]
            ++ static.data.body
    }


renderMarkdown markdown =
    case MarkdownHtmlRenderer.render markdown of
        Ok html ->
            html

        Err err ->
            [ Html.text "Error parsing blog post" ]
