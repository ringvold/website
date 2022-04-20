module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template, timestampView)

import Browser.Navigation
import Css
import Css.Global
import DataSource
import DateFormat
import Html exposing (Html)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css, id)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw
import Time exposing (Posix, Zone, utc)
import Timestamps exposing (Timestamps)
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Just OnPageChange
    }


type Msg
    = OnPageChange
        { path : Path
        , query : Maybe String
        , fragment : Maybe String
        }
    | SharedMsg SharedMsg


type alias Data =
    ()


type SharedMsg
    = NoOp


type alias Model =
    { showMobileMenu : Bool
    }


init :
    Maybe Browser.Navigation.Key
    -> Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Cmd Msg )
init navigationKey flags maybePagePath =
    ( { showMobileMenu = False }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange _ ->
            ( { model | showMobileMenu = False }, Cmd.none )

        SharedMsg globalMsg ->
            ( model, Cmd.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : DataSource.DataSource Data
data =
    DataSource.succeed ()


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : Html.Html msg, title : String }
view sharedData page model toMsg pageView =
    if not <| String.isEmpty <| Path.toRelative page.path then
        { body =
            div
                [ css
                    [ Tw.bg_gray_100
                    , Tw.font_sans
                    , Tw.leading_normal
                    , Tw.tracking_normal
                    , Tw.w_full
                    , Tw.h_full
                    , Tw.pb_4
                    ]
                ]
                [ Css.Global.global
                    (Tw.globalStyles
                        ++ [ Css.Global.body
                                [ Tw.bg_gray_100
                                ]
                           , Css.Global.selector "#footnotes li p"
                                [ Tw.py_2
                                ]
                           ]
                    )
                , nav
                , div
                    [ css
                        [ Breakpoints.md [ Tw.max_w_3xl ]
                        , Tw.mx_auto
                        , Css.fontFamilies [ Css.qt "Georgia", .value Css.serif ]
                        , Tw.container
                        ]
                    ]
                    [ div
                        [ css
                            [ Breakpoints.md [ Tw.px_6 ]
                            , Tw.px_4
                            , Tw.text_gray_800
                            , Tw.text_xl
                            , Tw.leading_normal
                            ]
                        ]
                        pageView.body
                    ]
                ]
                |> Html.Styled.toUnstyled
        , title = pageView.title
        }

    else
        { title = pageView.title
        , body =
            div [] pageView.body
                |> Html.Styled.toUnstyled
        }


nav : Html.Styled.Html msg
nav =
    Html.Styled.nav
        [ Attr.id "header"
        , css
            [ Tw.w_full
            , Tw.z_10
            , Tw.top_0
            , Tw.border_t_4
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Breakpoints.md [ Tw.max_w_4xl ]
                , Tw.mx_auto
                , Tw.w_full
                , Tw.flex_wrap
                , Tw.items_center
                , Tw.justify_between
                , Tw.mt_0
                , Tw.py_3
                ]
            ]
            [ div
                [ id "header-title", css [ Tw.pl_4 ] ]
                [ a
                    [ Attr.href <| Path.toAbsolute <| Route.toPath Route.Blog
                    , css
                        [ Tw.text_gray_900
                        , Tw.text_base
                        , Tw.no_underline
                        , Tw.font_extrabold
                        , Tw.text_xl
                        , Css.hover [ Tw.no_underline ]
                        , Tw.cursor_pointer
                        ]
                    ]
                    [ text "Harald Ringvold" ]
                ]
            , div
                [ id "collapsed-nav-content"
                , css [ Tw.pl_4 ]
                ]
                [ button [ id "nav-toggle" ] []
                ]
            , div
                [ id "nav-content"
                , css
                    [ Breakpoints.lg
                        [ Tw.flex
                        , Tw.items_center
                        , Tw.w_auto
                        , Tw.block
                        , Tw.mt_0
                        ]
                    , Breakpoints.md [ Tw.bg_transparent ]
                    , Tw.w_full
                    , Tw.flex_grow
                    , Tw.hidden
                    , Tw.mt_2
                    , Tw.z_20
                    , Tw.bg_gray_100
                    ]
                ]
                [ ul
                    [ css
                        [ Breakpoints.lg [ Tw.flex ]
                        , Tw.justify_end
                        , Tw.flex_1
                        , Tw.items_center
                        ]
                    ]
                    [--li [ css [ Tw.mr_3 ] ]
                     --    [ a
                     --        [ css
                     --            [ Tw.inline_block
                     --            , Tw.py_2
                     --            , Tw.px_4
                     --            , Tw.text_gray_900
                     --            , Tw.no_underline
                     --            , Tw.cursor_pointer
                     --            , Css.hover [ Tw.text_gray_900, Tw.underline ]
                     --            ]
                     --        ]
                     --        [ text "About" ]
                     --    ]
                    ]
                ]
            ]
        ]


timestampView : { a | timestamps : { b | created : Posix, updated : Posix } } -> Html.Styled.Html msg
timestampView entry =
    div
        [ css
            [ Tw.font_sans
            , Tw.text_sm
            , Tw.mb_5
            ]
        ]
        [ span [] [ text <| "Posted " ++ dateFormatter utc entry.timestamps.created ]
        , span [ css [ Tw.mx_2 ] ] [ text "|" ]
        , span [] [ text <| "Updated " ++ dateFormatter utc entry.timestamps.updated ]
        ]


dateFormatter : Zone -> Posix -> String
dateFormatter =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]
