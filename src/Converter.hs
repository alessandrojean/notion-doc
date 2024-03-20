{-# LANGUAGE OverloadedStrings #-}
module Converter where

import qualified NotionApiTypes as N
import qualified Data.Text as T
import qualified Text.Pandoc.Definition as P
import Text.Slugify (slugify)
import GHC.Exts (IsList(fromList))

createAst :: N.Page -> [N.Block] -> P.Pandoc
createAst page bks = P.Pandoc meta children
  where
    meta = createMeta page
    titleProp = N.propName $ N.pageProperties page
    slugH1 = slugify $ T.pack $ stripPlainText $ N.tpTitle titleProp
    header1 = P.Header 1 (slugH1, [], []) (parseInline $ N.tpTitle titleProp)
    children = header1 : createChildren bks

createMeta :: N.Page -> P.Meta
createMeta (N.Page _ properties date) = P.Meta $ fromList metadata
  where
    titleProp = N.propName properties
    title = T.pack $ stripPlainText $ N.tpTitle titleProp
    metadata = [
      ("title", P.MetaInlines [P.Str title]),
      ("date", P.MetaInlines [P.Str $ T.pack date])
      ]

createChildren :: [N.Block] -> [P.Block]
createChildren = nToP

nToP :: [N.Block] -> [P.Block]
nToP ((N.BlockBulletedListItem bli) : bs) = blocks
  where
    notionBlock = N.BlockBulletedListItem bli
    firstItem = notionToPandoc notionBlock
    nextItems = map ((\b -> [b]) . notionToPandoc) $ takeWhile isBli bs
    finalItems = drop (length nextItems) bs
    blocks = P.BulletList ([firstItem] : nextItems) : nToP finalItems
nToP ((N.BlockNumberedListItem nli) : bs) = blocks
  where
    attr = (1, P.Decimal, P.Period)
    notionBlock = N.BlockNumberedListItem nli
    firstItem = notionToPandoc notionBlock
    nextItems = map ((\b -> [b]) . notionToPandoc) $ takeWhile isNli bs
    finalItems = drop (length nextItems) bs
    blocks = P.OrderedList attr ([firstItem] : nextItems) : nToP finalItems
nToP ((N.BlockTableOfContents _) : bs) = createToc bs : nToP bs
nToP [] = []
nToP bs = notionToPandoc (head bs) : nToP (drop 1 bs)

createToc :: [N.Block] -> P.Block
createToc bs = P.OrderedList (1, P.Decimal, P.Period) items
  where
    items = map convertH2 $ filter isH2 bs
    convertH2 b = [P.Plain $ parseInline $ N.hdRichText $ N.heading2 b]

isH2 :: N.Block -> Bool
isH2 (N.BlockHeading2 _) = True
isH2 _ = False

isBli :: N.Block -> Bool
isBli (N.BlockBulletedListItem _) = True
isBli _ = False

isNli :: N.Block -> Bool
isNli (N.BlockNumberedListItem _) = True
isNli _ = False

notionToPandoc :: N.Block -> P.Block
notionToPandoc (N.BlockParagraph pr) = P.Para (parseText pr)
notionToPandoc (N.BlockHeading1 h1) = P.Header 1 P.nullAttr (parseHeading h1)
notionToPandoc (N.BlockHeading2 h2) = P.Header 2 P.nullAttr (parseHeading h2)
notionToPandoc (N.BlockHeading3 h3) = P.Header 3 P.nullAttr (parseHeading h3)
notionToPandoc (N.BlockBulletedListItem bli) = P.Plain (parseText bli)
notionToPandoc (N.BlockNumberedListItem nli) = P.Plain (parseText nli)
notionToPandoc (N.BlockQuote qt _) = P.BlockQuote [P.Para (parseText qt)]
notionToPandoc (N.BlockCode cd) = P.CodeBlock attr text
  where
    attr = ("", [T.pack $ N.codeLanguage cd], [])
    text = T.pack $ stripPlainText $ N.codeRichText cd
notionToPandoc (N.BlockImage img) = P.Figure P.nullAttr caption [P.Plain [image]]
  where
    caption = imageCaption img
    inl = imageInline img
    image = P.Image P.nullAttr inl (imageUrl img, "fig:")
notionToPandoc (N.BlockCallout cal _) = P.Div attr blocks
  where
    attr = ("", [calloutType $ N.caIcon cal], [])
    blocks =
      [ P.Div ("", ["title"], []) []
      , P.Para (parseInline $ N.caRichText cal)
      ]
notionToPandoc N.BlockDivider = P.HorizontalRule
notionToPandoc _ = P.Para [P.Str "Unsupported block"]

calloutType :: N.Icon -> T.Text
calloutType (N.IconEmoji "ℹ️") = "note"
calloutType (N.IconEmoji "💡") = "tip"
calloutType (N.IconEmoji "🛑") = "caution"
calloutType (N.IconEmoji "‼️") = "important"
calloutType (N.IconEmoji "⚠️") = "warning"
calloutType _ = ""

imageCaption :: N.Image -> P.Caption
imageCaption (N.ImageExternal _ capt) = P.Caption Nothing [P.Plain (parseInline capt)]
imageCaption (N.ImageFile _ capt) = P.Caption Nothing [P.Plain (parseInline capt)]

imageInline :: N.Image -> [P.Inline]
imageInline (N.ImageExternal _ capt) = parseInline capt
imageInline (N.ImageFile _ capt) = parseInline capt

imageUrl :: N.Image -> T.Text
imageUrl (N.ImageExternal ext _) = T.pack $ N.erUrl ext
imageUrl (N.ImageFile file _) = T.pack $ N.flUrl file

parseText :: N.Text -> [P.Inline]
parseText txt = parseInline $ N.txRichText txt

parseHeading :: N.Heading -> [P.Inline]
parseHeading hd = parseInline $ N.hdRichText hd

parseInline :: [N.RichTextItem] -> [P.Inline]
parseInline = map parseRichTextItem

parseRichTextItem :: N.RichTextItem -> P.Inline
parseRichTextItem (N.TextRichText txt href an _) = linkify href $ annotate txt an
parseRichTextItem (N.EquationRichText eq _ _ _) = P.Math P.InlineMath $ T.pack eq

linkify :: Maybe String -> P.Inline -> P.Inline
linkify Nothing txt = txt
linkify (Just href) txt = P.Link P.nullAttr [txt] (T.pack href, "")

annotate :: String -> N.AnnotationResponse -> P.Inline
annotate txt an | N.bold an = P.Strong [annotate txt (an { N.bold = False })]
                | N.italic an = P.Emph [annotate txt (an { N.italic = False })]
                | N.strikethrough an = P.Strikeout [annotate txt (an { N.strikethrough = False })]
                | N.underline an = P.Underline [annotate txt (an { N.underline = False })]
                | N.mono an = P.Code P.nullAttr (T.pack txt)
                | otherwise = P.Str (T.pack txt)

stripPlainText :: [N.RichTextItem] -> String
stripPlainText txt = unwords $ map ptxt txt
  where
    ptxt :: N.RichTextItem -> String
    ptxt (N.TextRichText tx _ _ _) = tx
    ptxt (N.EquationRichText tx _ _ _) = tx
