module Page.Blog.Slug_ exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html
import Markdown
import OptimizedDecoder
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Posts
import Shared
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
                DataSource.File.bodyWithFrontmatter
                    (\body ->
                        OptimizedDecoder.map
                            (Data body)
                            (OptimizedDecoder.field "title" OptimizedDecoder.string)
                    )
                    filePath
            )


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


type alias Data =
    { body : String
    , title : String
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = static.routeParams.slug
    , body =
        [ Html.h1 [] [ Html.text static.data.title ]
        , Markdown.toHtml [] static.data.body
        ]
    }
