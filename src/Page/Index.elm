module Page.Index exposing (Data, Model, Msg, page)

import Css
import Css.Global
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Link
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Route exposing (Route(..))
import Shared
import Tailwind.Utilities as Tw
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


data : DataSource Data
data =
    DataSource.succeed ()


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Harald Ringvold"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "ringvold.io logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Harald Ringvold. Software developer."
        , locale = Nothing
        , title = "ringvold.io"
        }
        |> Seo.website


type alias Data =
    ()


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ _ =
    { title = ""
    , body =
        [ Css.Global.global
            [ Css.Global.selector "body"
                [ Css.property "margin" "0"
                , Css.property "padding" "0"
                ]
            ]
        , div
            [ Attr.class "frontpage"
            , css
                [ Tw.bg_frontpage
                , Tw.bg_cover
                , Tw.flex
                , Tw.items_center
                , Tw.h_screen
                , Tw.bg_gray_800
                ]
            ]
            [ div
                [ css
                    [ Tw.items_center
                    , Tw.w_full
                    , Tw.text_center
                    , Css.fontFamilies [ Css.qt "Source Sans Pro", .value Css.sansSerif ]
                    , Tw.text_white
                    ]
                ]
                [ h1
                    [-- css
                     --    [ Tw.text_7xl
                     --    , Tw.my_4
                     --    , Tw.font_medium
                     --    , Tw.opacity_40
                     --    ]
                    ]
                    [ text "Harald Ringvold" ]
                , h2
                    [-- css
                     --    [ Tw.text_5xl
                     --    , Tw.font_light
                     --    , Tw.text_frontpageH2
                     --    , Tw.mt_3
                     --    , Tw.mb_10
                     --    ]
                    ]
                    [ text "Developer" ]
                , a
                    [ Attr.class "button-white"

                    --, linkStyle
                    ]
                    [ text "Github" ]
                , a
                    [ Attr.class "button-white"

                    --, linkStyle
                    ]
                    [ text "Twitter" ]
                , Link.link Route.Blog
                    [ Attr.class "button-white"
                    , css [ Tw.no_underline ]
                    ]
                  <|
                    text "Blog"
                ]
            ]
        ]
    }



-- border-radius: 20px;
-- color: #333;
-- padding: 10px 20px;
-- margin: 0 10px;
-- letter-spacing: 2px;
-- background: #FFF;
-- font-weight: 600;
-- border: 3px solid #FFF;
-- cursor: pointer;
-- text-transform: uppercase;
-- font-size: 12px;
-- text-align: center;
