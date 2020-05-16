module FrontPage exposing (buttonStyle, view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html exposing (Html)
import Html.Attributes as Attr
import Pages exposing (images, pages)
import Pages.ImagePath as ImagePath exposing (ImagePath)


view =
    Element.column
        [ Element.centerX
        , Element.centerY
        ]
        [ Element.text "Harald Ringvold"
            |> Element.el
                [ Element.Region.heading 1
                , Element.centerX
                , Element.centerY
                , Element.paddingXY 15 0
                , Font.size 69
                , Font.color <| Element.rgba255 255 255 255 0.3 -- #303333
                ]
        , Element.text "Software developer"
            |> Element.el
                [ Element.Region.heading 2
                , Element.centerX
                , Element.centerY
                , Element.paddingEach
                    { top = 25
                    , right = 0
                    , bottom = 46
                    , left = 0
                    }
                , Font.size 46
                , Font.extraLight
                , Font.color (Element.rgb255 85 85 85) -- #555555
                ]
        , Element.row [ Element.centerX, Element.spacing 20 ]
            [ Element.link buttonStyle
                { url = "/blog"
                , label = Element.text "Blog"
                }
            , Element.link buttonStyle
                { url = "https://github.com/ringvold"
                , label = Element.text "Github"
                }
            , Element.link buttonStyle
                { url = "https://twitter.com/hringvold"
                , label = Element.text "Twitter"
                }
            ]
        ]
        |> Element.layout
            [ Background.image <| ImagePath.toString images.bg
            , Element.width Element.fill
            , Element.height Element.fill
            , Font.size 20
            , Font.family [ Font.typeface "Source Sans Pro", Font.sansSerif ]
            , Background.color (Element.rgb255 42 49 55) -- #2A3137
            ]


buttonStyle =
    [ Background.color (Element.rgb 1 1 1)
    , Border.color (Element.rgb 1 1 1)
    , Border.rounded 20
    , Border.width 3
    , Element.paddingXY 20 10
    , Element.htmlAttribute <| Attr.class "button-white"
    , Font.color (Element.rgb255 51 51 51)
    , Font.size 12
    , Font.letterSpacing 2
    , Font.semiBold
    ]
