module Page.Blog exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Link
import MarkdownCodec
import MarkdownHtmlRenderer
import MarkdownHtmlRenderer2
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
    { title : String
    , route : Route
    , date : String
    , excerpt : String
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
                    MarkdownCodec.titleAndDescription filePath
                        |> DataSource.map
                            (\{ title, description } ->
                                { title = title
                                , route = Route.Blog__Slug_ { slug = slug }
                                , date = "Yesterday"
                                , excerpt = String.left 80 description ++ "..."
                                }
                            )
                )
                >> DataSource.combine
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
            [ Link.link entry.route [] (text entry.title) ]
        , p [] [ text entry.excerpt ]
        ]
