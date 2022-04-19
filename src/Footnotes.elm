module Footnotes exposing (gatherFootnotes)

import Dict
import Markdown.Block as Block exposing (..)
import Tailwind.Utilities exposing (block)


gatherFootnotes : List Block -> List Block
gatherFootnotes blocks =
    let
        footnoteRefs : Dict.Dict String ( Int, Inline )
        footnoteRefs =
            blocks
                |> inlineFoldl
                    (\inline ( count, dict ) ->
                        case inline of
                            Block.HtmlInline (HtmlElement "fn-ref" attrs _) ->
                                let
                                    text =
                                        attrs
                                            |> List.filter (.name >> (==) "id")
                                            |> List.map .value
                                            |> List.head
                                            |> Maybe.withDefault ""

                                    updatedCount =
                                        count + 1
                                in
                                ( updatedCount, Dict.insert text ( updatedCount, inline ) dict )

                            _ ->
                                ( count, dict )
                    )
                    ( 0, Dict.empty )
                |> Tuple.second

        ( footnotes, updatedBlocks ) =
            gatherAndMap footnoteRefs blocks

        updatedFootnotes =
            Dict.values footnotes
                |> List.map
                    (\block ->
                        case block of
                            HtmlBlock (HtmlElement "fn" attrs children) ->
                                HtmlBlock (HtmlElement "li" attrs children)

                            _ ->
                                block
                    )
                |> (\fts ->
                        [ Block.ThematicBreak
                        , Block.HtmlBlock
                            (Block.HtmlElement "footnote-section"
                                []
                                fts
                            )
                        ]
                   )
    in
    updatedBlocks ++ updatedFootnotes


gatherAndMap : Dict.Dict String ( Int, Inline ) -> List Block -> ( Dict.Dict String Block, List Block )
gatherAndMap footnoteRefs blocks =
    blocks
        |> myMapAndAccumulate
            (\soFar block ->
                case block of
                    HtmlBlock (HtmlElement "fn" attrs _) ->
                        let
                            text =
                                attrs
                                    |> List.filter (.name >> (==) "id")
                                    |> List.map .value
                                    |> List.head
                                    |> Maybe.withDefault ""
                        in
                        ( Dict.insert text block soFar
                        , Block.HtmlBlock (Block.HtmlComment "removed fn tag")
                        )

                    Paragraph inlines ->
                        ( soFar
                        , Block.Paragraph <| mapInlines inlines footnoteRefs
                        )

                    _ ->
                        ( soFar, block )
            )
            Dict.empty


mapInlines : List Inline -> Dict.Dict String ( Int, Inline ) -> List Inline
mapInlines inlines footnoteRefs =
    inlines
        |> List.map
            (\inline ->
                case inline of
                    HtmlInline (HtmlElement "fn-ref" attrs innies) ->
                        let
                            text =
                                attrs
                                    |> List.filter (.name >> (==) "id")
                                    |> List.map .value
                                    |> List.head
                                    |> Maybe.withDefault ""

                            occurencesNr =
                                footnoteRefs
                                    |> Dict.get text
                                    |> Maybe.map Tuple.first
                                    |> Maybe.map String.fromInt
                        in
                        case occurencesNr of
                            Just nr ->
                                HtmlInline
                                    (HtmlElement "fn-ref"
                                        ({ name = "count", value = nr } :: attrs)
                                        innies
                                    )

                            Maybe.Nothing ->
                                inline

                    HtmlInline (HtmlElement "fn" _ _) ->
                        HtmlInline (Block.HtmlComment "Inline fn")

                    _ ->
                        inline
            )


myMapAndAccumulate : (soFar -> Block -> ( soFar, mappedValue )) -> soFar -> List Block -> ( soFar, List mappedValue )
myMapAndAccumulate mapFn initialValue blocks =
    let
        ( accFinal, generatedList ) =
            myFoldl
                (\block ( acc1, ys ) ->
                    let
                        ( acc2, mappedBlock ) =
                            mapFn acc1 block
                    in
                    ( acc2, mappedBlock :: ys )
                )
                ( initialValue, [] )
                blocks
    in
    ( accFinal, List.reverse generatedList )


myFoldl : (Block -> acc -> acc) -> acc -> List Block -> acc
myFoldl function acc list =
    case list of
        [] ->
            acc

        block :: remainingBlocks ->
            case block of
                HtmlBlock html ->
                    myFoldl function (function block acc) remainingBlocks

                UnorderedList tight blocks ->
                    let
                        childBlocks : List Block
                        childBlocks =
                            blocks
                                |> List.concatMap (\(ListItem _ children) -> children)
                    in
                    myFoldl function (function block acc) remainingBlocks

                OrderedList _ int blocks ->
                    myFoldl function (function block acc) remainingBlocks

                BlockQuote blocks ->
                    myFoldl function (function block acc) remainingBlocks

                -- These cases don't have nested blocks
                -- So no recursion needed
                Heading _ _ ->
                    myFoldl function (function block acc) remainingBlocks

                Paragraph _ ->
                    myFoldl function (function block acc) remainingBlocks

                Table _ _ ->
                    myFoldl function (function block acc) remainingBlocks

                CodeBlock _ ->
                    myFoldl function (function block acc) remainingBlocks

                ThematicBreak ->
                    myFoldl function (function block acc) remainingBlocks
