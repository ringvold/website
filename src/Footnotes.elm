module Footnotes exposing (gatherFootnotes)

import Dict
import Markdown.Block as Block exposing (..)
import Tailwind.Utilities exposing (block)



{-
   Usage:
   Where you want to reference a footnote ([1]): `<fn-ref id="some-note-id">`
   Some where in the document you write your footnote like this: `<fn id="some-note-id">This is the footnote. Lets add some mores stuff here</fn>`
   These note will be moved to the end of the document.
-}


type alias Footnote =
    { id : String
    , index : Int
    , block : Block
    }


type alias FootnoteRef =
    ( Int, Inline )


{-| In elm-markdown some html elements are classified as Blocks and some are Inlines.
For our purpose `fn-ref`s are HtmlInlines as they allwasy will be inside other
html elements and `fn`s are considered Blocks as one most likely will put the
notes definitions them self on root level. Todo: Add support for notes defined inline.
The Block list is a tokenized version of the markdown.

The following is a simplified version of what happens in gatherFootnotes:

1.  Finds all the footnote refs
2.  Use the refs to identify and match the notes (fn-tags) and remove those blocks from the block list and into a separate list
3.  In the same pass we find and add some metadata to the fn-ref-tags so we can render them as numbers in brackets
4.  Finally the removed fn blocks are added back at the end of the block list

This function should probably have a better name.

-}
gatherFootnotes : List Block -> List Block
gatherFootnotes blocks =
    let
        footnoteRefs : Dict.Dict String FootnoteRef
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

        footnoteSection =
            Dict.values footnotes
                |> List.sortBy (\note -> note.index)
                |> List.map
                    (\note ->
                        case note.block of
                            HtmlBlock (HtmlElement "fn" attrs children) ->
                                HtmlBlock (HtmlElement "fn" ({ name = "ftnt", value = "true" } :: attrs) children)

                            _ ->
                                note.block
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
    updatedBlocks ++ footnoteSection


gatherAndMap : Dict.Dict String FootnoteRef -> List Block -> ( Dict.Dict String Footnote, List Block )
gatherAndMap footnoteRefs blocks =
    blocks
        |> altMapAndAccumulate
            (\gatheredFootnotes block ->
                case block of
                    HtmlBlock (HtmlElement "fn" attrs _) ->
                        let
                            footnoteId =
                                attrs
                                    |> List.filter (.name >> (==) "id")
                                    |> List.map .value
                                    |> List.head
                                    |> Maybe.withDefault ""

                            occurencesNr =
                                footnoteRefs
                                    |> Dict.get footnoteId
                                    |> Maybe.map Tuple.first
                                    |> Maybe.withDefault 0

                            footnote =
                                { id = footnoteId
                                , index = occurencesNr
                                , block = block
                                }
                        in
                        ( Dict.insert footnoteId footnote gatheredFootnotes
                        , Block.HtmlBlock (Block.HtmlComment "removed fn tag")
                        )

                    Paragraph inlines ->
                        ( gatheredFootnotes
                        , Block.Paragraph <| mapInlines inlines footnoteRefs
                        )

                    _ ->
                        ( gatheredFootnotes, block )
            )
            Dict.empty


mapInlines : List Inline -> Dict.Dict String FootnoteRef -> List Inline
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


{-| Alternative mapAndAccumulate.
This is mapAndAccumulate from Markdown.Block but uses custom fold function
-}
altMapAndAccumulate : (soFar -> Block -> ( soFar, mappedValue )) -> soFar -> List Block -> ( soFar, List mappedValue )
altMapAndAccumulate mapFn initialValue blocks =
    let
        ( accFinal, generatedList ) =
            altFoldl
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


{-| This version of foldl does not add child blocks to remainingBlocks, effectivly
identifying footnotes and removing fn-tags in one pass.
Can not remember exacly why it is done this way but it works 🤷
-}
altFoldl : (Block -> acc -> acc) -> acc -> List Block -> acc
altFoldl function acc list =
    case list of
        [] ->
            acc

        block :: remainingBlocks ->
            altFoldl function (function block acc) remainingBlocks
