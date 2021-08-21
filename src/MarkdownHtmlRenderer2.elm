module MarkdownHtmlRenderer2 exposing (..)

import Css
import DataSource exposing (DataSource)
import DataSource.Port
import Html.Attributes
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)
import Json.Encode
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Markdown.Scaffolded as Scaffolded exposing (..)
import OptimizedDecoder as Decode
import Parser
import Parser.Advanced
import Shiki
import Site
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw


renderer : Markdown.Renderer.Renderer (DataSource (Html msg))
renderer =
    Scaffolded.toRenderer
        { renderHtml = Markdown.Html.oneOf []
        , renderMarkdown = Scaffolded.withDataSource reduceHtmlDataSource
        }


reduceHtmlDataSource : Block (Html msg) -> DataSource (Html msg)
reduceHtmlDataSource block =
    case block of
        Scaffolded.Paragraph children ->
            Html.p
                [ css
                    [ Tw.py_6
                    ]
                ]
                children
                |> DataSource.succeed

        Scaffolded.Heading { rawText, level, children } ->
            (case level of
                Block.H1 ->
                    Html.h1
                        [ css
                            [ Tw.py_2
                            , Tw.font_sans
                            ]
                        ]
                        children

                Block.H2 ->
                    Html.h2
                        [ Attr.id (rawTextToId rawText)
                        , Attr.attribute "name" (rawTextToId rawText)
                        , css
                            [ Tw.py_2
                            , Tw.font_sans
                            ]
                        ]
                        [ Html.a
                            [ Attr.href <| "#" ++ rawTextToId rawText
                            , css
                                [ Tw.no_underline |> Css.important
                                ]
                            ]
                            (children
                                ++ [ Html.span
                                        [ Attr.class "anchor-icon"
                                        , css
                                            [ Tw.ml_2
                                            , Tw.text_gray_400
                                            , Tw.select_none
                                            ]
                                        ]
                                        [ Html.text "#" ]
                                   ]
                            )
                        ]

                _ ->
                    (case level of
                        Block.H1 ->
                            Html.h1

                        Block.H2 ->
                            Html.h2

                        Block.H3 ->
                            Html.h3

                        Block.H4 ->
                            Html.h4

                        Block.H5 ->
                            Html.h5

                        Block.H6 ->
                            Html.h6
                    )
                        [ css
                            [ Tw.py_2
                            , Tw.font_sans
                            ]
                        ]
                        children
            )
                |> DataSource.succeed

        Scaffolded.CodeBlock info ->
            shikiDataSource info

        --SyntaxHighlight.elm body
        --    |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
        --    |> Result.map Html.fromUnstyled
        --    |> Result.withDefault
        --        (Html.pre
        --            [ css
        --                [ Tw.border_8 |> Css.important
        --                ]
        --            ]
        --            [ Html.code [] [ Html.text details.body ] ]
        --        )
        --    |> List.singleton
        --    |> Html.div
        --        [ css
        --            [ Tw.mt_8
        --            , Tw.mb_8
        --            ]
        --        ]
        --    |> DataSource.succeed
        Scaffolded.Text string ->
            DataSource.succeed (Html.text string)

        Scaffolded.Emphasis content ->
            DataSource.succeed (Html.em [ css [ Tw.italic ] ] content)

        Scaffolded.Strong content ->
            Html.strong [ css [ Tw.font_bold ] ] content
                |> DataSource.succeed

        Scaffolded.BlockQuote children ->
            Html.blockquote [] children
                |> DataSource.succeed

        Scaffolded.CodeSpan content ->
            Html.code
                [ css
                    [ Tw.font_semibold
                    , Tw.font_medium
                    , Css.color (Css.rgb 226 0 124) |> Css.important
                    ]
                ]
                [ Html.text content ]
                |> DataSource.succeed

        Scaffolded.Strikethrough children ->
            Html.del [] children
                |> DataSource.succeed

        Link { destination, title, children } ->
            Html.a
                [ Attr.href <| slugToAbsoluteUrl destination
                , css
                    [ Tw.no_underline
                    , Tw.font_bold
                    , Tw.text_green_500
                    , Css.hover
                        [ Tw.underline
                        ]
                    ]
                ]
                (title
                    |> Maybe.map Html.text
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault children
                )
                |> DataSource.succeed

        Image image ->
            case image.title of
                Just _ ->
                    Html.img [ Attr.src image.src, Attr.alt image.alt ] []
                        |> DataSource.succeed

                Nothing ->
                    Html.img [ Attr.src image.src, Attr.alt image.alt ] []
                        |> DataSource.succeed

        UnorderedList { items } ->
            Html.ul
                [ css
                    [ Tw.list_disc
                    , Tw.mb_5
                    , Tw.mt_5
                    ]
                ]
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
                                    Html.li
                                        [ css
                                            [ Tw.ml_7
                                            , Tw.mb_2
                                            , Tw.mt_2
                                            ]
                                        ]
                                        (checkbox :: children)
                        )
                )
                |> DataSource.succeed

        OrderedList { startingIndex, items } ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attr.start startingIndex
                        , css
                            [ Tw.list_decimal
                            , Tw.list_inside
                            , Tw.mt_5
                            , Tw.mb_5
                            ]
                        ]

                    _ ->
                        [ css
                            [ Tw.list_decimal
                            , Tw.list_inside
                            , Tw.mt_5
                            , Tw.mb_5
                            ]
                        ]
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
                |> DataSource.succeed

        ThematicBreak ->
            Html.hr [] []
                |> DataSource.succeed

        HardLineBreak ->
            Html.br [] []
                |> DataSource.succeed

        Table children ->
            Html.table [] children
                |> DataSource.succeed

        TableHeader children ->
            Html.thead [] children
                |> DataSource.succeed

        TableBody children ->
            Html.tbody [] children
                |> DataSource.succeed

        TableRow children ->
            Html.tr [] children
                |> DataSource.succeed

        TableCell maybeAlignment children ->
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
            Html.td attrs children
                |> DataSource.succeed

        TableHeaderCell maybeAlignment children ->
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
            Html.th attrs children
                |> DataSource.succeed


shikiDataSource : { body : String, language : Maybe String } -> DataSource (Html msg)
shikiDataSource info =
    DataSource.Port.get "highlight"
        (Json.Encode.object
            [ ( "body", Json.Encode.string info.body )
            , ( "language"
              , info.language
                    |> Maybe.map Json.Encode.string
                    |> Maybe.withDefault Json.Encode.null
              )
            ]
        )
        (Shiki.decoder
            |> Decode.map
                (Shiki.view
                    [ Html.Attributes.style "font-family" "IBM Plex Mono"
                    , Html.Attributes.style "padding" "0.75rem 1.25rem"
                    , Html.Attributes.style "font-size" "13px"
                    , Html.Attributes.style "border-radius" "0.5rem"
                    ]
                )
            |> Decode.map Html.fromUnstyled
        )


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


slugToAbsoluteUrl : String -> String
slugToAbsoluteUrl slugOrUrl =
    if slugOrUrl |> String.contains "/" then
        slugOrUrl

    else
        "/" ++ slugOrUrl
