module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import Browser.Navigation
import Css
import Css.Global
import DataSource
import Html exposing (Html)
import Html.Styled exposing (a, div, text)
import Html.Styled.Attributes as Attr exposing (css)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw
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
    -> { body : Html msg, title : String }
view sharedData page model toMsg pageView =
    { body =
        div
            [ css
                [ Tw.bg_gray_100
                , Tw.font_sans
                , Tw.leading_normal
                , Tw.tracking_normal
                , Tw.w_full
                , Tw.h_full

                -- , Tw.w_screen
                -- , Tw.h_screen
                ]
            ]
            [ Css.Global.global Tw.globalStyles
            , nav
            , div
                [ css
                    [ Tw.container
                    , Breakpoints.md [ Tw.max_w_3xl ]
                    , Tw.mx_auto
                    , Tw.pt_20
                    , Css.fontFamilies [ Css.qt "Georgia", .value Css.serif ]
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


nav : Html.Styled.Html msg
nav =
    Html.Styled.nav
        [ Attr.id "header"
        , css [ Tw.fixed, Tw.w_full, Tw.z_10, Tw.top_0 ]
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
                [ css [ Tw.pl_4 ] ]
                [ a
                    [ css
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
            ]
        ]
