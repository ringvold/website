module MarkdownHtmlRenderer exposing (..)

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Parser
import Parser.Advanced
import Site
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw


render : String -> Result String (List (Html msg))
render markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Markdown.Renderer.render customRenderer ast)


deadEndsToString : List (Parser.Advanced.DeadEnd String Parser.Problem) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"


customRenderer : Markdown.Renderer.Renderer (Html msg)
customRenderer =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    Html.h1 [ css [ Tw.py_2, Tw.font_sans ] ] children

                Block.H2 ->
                    Html.h2 [ css [ Tw.py_2, Tw.font_sans ] ] children

                Block.H3 ->
                    Html.h3 [ css [ Tw.py_2, Tw.font_sans ] ] children

                Block.H4 ->
                    Html.h4 [ css [ Tw.py_2, Tw.font_sans ] ] children

                Block.H5 ->
                    Html.h5 [ css [ Tw.py_2, Tw.font_sans ] ] children

                Block.H6 ->
                    Html.h6 [ css [ Tw.py_2, Tw.font_sans ] ] children
    , paragraph = Html.p [ css [ Tw.py_6 ] ]
    , strikethrough = Html.del []
    , hardLineBreak = Html.br [] []
    , blockQuote =
        Html.blockquote
            [ css
                [ Tw.border_l_4
                , Tw.border_green_500
                , Tw.italic
                , Tw.my_8
                , Breakpoints.md [ Tw.pl_12 ]
                , Tw.pl_8
                ]
            ]
    , strong =
        \children -> Html.strong [ css [ Tw.font_bold ] ] children
    , emphasis =
        \children -> Html.em [] children
    , codeSpan =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            let
                fullUrl =
                    if link.destination |> String.startsWith "/" then
                        Site.config.canonicalUrl ++ link.destination

                    else
                        link.destination
            in
            case link.title of
                Just title ->
                    Html.a
                        [ Attr.href fullUrl
                        , Attr.title title
                        , css
                            [ Tw.text_green_500
                            , Tw.no_underline
                            , Css.hover [ Tw.underline ]
                            ]
                        ]
                        content

                Nothing ->
                    Html.a
                        [ Attr.href fullUrl
                        , css
                            [ Tw.text_green_500
                            , Tw.no_underline
                            , Css.hover [ Tw.underline ]
                            ]
                        ]
                        content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        , Attr.title title
                        ]
                        []

                Nothing ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        ]
                        []
    , text =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (if startingIndex /= 1 then
                    [ Attr.start startingIndex ]

                 else
                    []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html =
        Markdown.Html.oneOf
            []
    , codeBlock =
        \{ body, language } ->
            Html.pre []
                [ Html.code []
                    [ Html.text body
                    ]
                ]
    , thematicBreak = Html.hr [] []
    , table = Html.table []
    , tableHeader = Html.thead []
    , tableBody = Html.tbody []
    , tableRow = Html.tr []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.th attrs
    , tableCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.td attrs
    }
