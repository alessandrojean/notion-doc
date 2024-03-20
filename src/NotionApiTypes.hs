{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module NotionApiTypes where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Aeson.Casing (aesonPrefix, snakeCase, pascalCase, aesonDrop)
import qualified Data.Char as Char
import Utils (dropLast, fixBlockType)
  
data Page = Page 
  { pageId :: String
  , pageProperties :: PageProperties
  , pageCreatedTime :: String
  } deriving (Show, Generic)

instance FromJSON Page where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype PageProperties = PageProperties { propName :: TitleProperty }
  deriving (Show, Generic)

instance FromJSON PageProperties where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

newtype TitleProperty = TitleProperty { tpTitle :: [RichTextItem] }
  deriving (Show, Generic)

instance FromJSON TitleProperty where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data PageBlocks = PageBlocks
  { results :: [Block]
  , nextCursor :: Maybe String
  , hasMore :: Bool
  } deriving (Show, Generic)

instance FromJSON PageBlocks where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data RichTextItem = 
  TextRichText
    { txPlainText :: String
    , txHref :: Maybe String
    , txAnnotations :: AnnotationResponse
    , txText :: RichText
    }
  | EquationRichText
    { eqPlainText :: String
    , eqEquation :: Equation
    , eqAnnotations :: AnnotationResponse
    , eqHref :: Maybe String
    } deriving (Show, Generic)

richTextOptions :: Options
richTextOptions = defaultOptions 
  { sumEncoding = TaggedObject { tagFieldName = "type", contentsFieldName = undefined }
  , constructorTagModifier = dropLast 8 . map Char.toLower
  , fieldLabelModifier = snakeCase . drop 2
  }

instance FromJSON RichTextItem where
  parseJSON = genericParseJSON richTextOptions

newtype Equation = Equation { expression :: String } deriving (Show, Generic)
instance FromJSON Equation

data AnnotationResponse = AnnotationResponse
  { bold :: Bool
  , italic :: Bool
  , strikethrough :: Bool
  , underline :: Bool
  , mono :: Bool
  , color :: String
  } deriving (Show)
instance FromJSON AnnotationResponse where
  parseJSON = withObject "AnnotationResponse" $ \v -> AnnotationResponse
    <$> v .: "bold"
    <*> v .: "italic"
    <*> v .: "strikethrough"
    <*> v .: "underline"
    <*> v .: "code"
    <*> v .: "color"

data RichText = RichText
  { content :: String
  , link :: Maybe RichTextLink
  } deriving (Show, Generic)
instance FromJSON RichText

newtype RichTextLink = RichTextLink { url :: String } deriving (Show, Generic)
instance FromJSON RichTextLink

data Block = BlockParagraph { paragraph :: Text }
  | BlockHeading1 { heading1 :: Heading }
  | BlockHeading2 { heading2 :: Heading }
  | BlockHeading3 { heading3 :: Heading }
  | BlockBulletedListItem { bulletedListItem :: Text }
  | BlockNumberedListItem { numberedListItem :: Text }
  | BlockQuote { quote :: Text, hasChildren :: Bool }
  | BlockToDo { toDo :: ToDo }
  | BlockToggle { toggle :: Text }
  | BlockEquation { equation :: Equation }
  | BlockCode { code :: Code }
  | BlockCallout { callout :: Callout, hasChildren :: Bool }
  | BlockDivider
  | BlockTableOfContents { tableOfContents :: TableOfContents }
  | BlockImage { image :: Image }
  deriving (Show, Generic)

blockOptions :: Options
blockOptions = defaultOptions 
  { sumEncoding = TaggedObject { tagFieldName = "type", contentsFieldName = undefined }
  , constructorTagModifier = snakeCase . fixBlockType . drop 5
  , fieldLabelModifier = snakeCase . fixBlockType
  }

instance FromJSON Block where
  parseJSON = genericParseJSON blockOptions

data Text = Text
  { txRichText :: [RichTextItem]
  , txColor :: String
  } deriving (Show, Generic)
instance FromJSON Text where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Heading = Heading
  { hdRichText :: [RichTextItem]
  , hdColor :: String
  , hdIsToggleable :: Bool
  } deriving (Show, Generic)
instance FromJSON Heading where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ToDo = ToDo
  { tdRichText :: [RichTextItem]
  , tdColor :: String
  , tdChecked :: Bool
  } deriving (Show, Generic)
instance FromJSON ToDo where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Code = Code
  { codeRichText :: [RichTextItem]
  , codeCaption :: [RichTextItem]
  , codeLanguage :: String
  } deriving (Show, Generic)
instance FromJSON Code where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Callout = Callout
  { caRichText :: [RichTextItem]
  , caColor :: String
  , caIcon :: Icon
  } deriving (Show, Generic)
instance FromJSON Callout where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Icon = IconEmoji { icEmEmoji :: String }
  | IconExternal { icErUrl :: String }
  | IconFile { icFlUrl :: String }
  deriving (Show, Generic)

iconOptions :: Options
iconOptions = defaultOptions 
  { sumEncoding = TaggedObject { tagFieldName = "type", contentsFieldName = undefined }
  , constructorTagModifier = map Char.toLower . drop 4
  , fieldLabelModifier = snakeCase . drop 4
  }

instance FromJSON Icon where
  parseJSON = genericParseJSON iconOptions

newtype TableOfContents = TableOfContents { tocColor :: String } deriving (Show, Generic)
instance FromJSON TableOfContents where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Image = ImageExternal { imgErExternal :: ExternalResource, imgErCaption :: [RichTextItem] }
  | ImageFile { imgFlFile :: FileResource, imgFlCaption :: [RichTextItem] }
  deriving (Show, Generic)

newtype ExternalResource = ExternalResource { erUrl :: String } deriving (Show, Generic)
instance FromJSON ExternalResource where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data FileResource = FileResource 
  { flUrl :: String
  , flExpiryTime :: String
  } deriving (Show, Generic)
instance FromJSON FileResource where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase


imageOptions :: Options
imageOptions = defaultOptions 
  { sumEncoding = TaggedObject { tagFieldName = "type", contentsFieldName = undefined }
  , constructorTagModifier = map Char.toLower . drop 5
  , fieldLabelModifier = snakeCase . drop 5
  }

instance FromJSON Image where
  parseJSON = genericParseJSON imageOptions

