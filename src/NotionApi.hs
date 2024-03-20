{-# LANGUAGE OverloadedStrings #-}
module NotionApi (
  retrieveNotionPage,
  retrieveBlockChildren,
  retrieveAllBlockChildren
) where

import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Conduit
import Network.HTTP.Simple
import NotionApiTypes (Page, PageBlocks, Block, results, nextCursor)

baseUrl :: String
baseUrl = "https://api.notion.com/v1"

apiVersion :: S8.ByteString
apiVersion = "2022-06-28"

apiKey :: S8.ByteString
apiKey = "secret"

-- https://developers.notion.com/reference/retrieve-a-page
retrieveNotionPage :: String -> IO Page
retrieveNotionPage pageToFetch = do
  -- putStrLn "Fetching the page information."
  initialRequest <- parseRequest $ baseUrl ++ "/pages/" ++ pageToFetch
  let request
        = setRequestMethod "GET"
        $ addRequestHeader "Notion-Version" apiVersion
        $ setRequestBearerAuth apiKey initialRequest

  response <- httpJSON request
  return $ responseBody response

-- https://developers.notion.com/reference/get-block-children
retrieveBlockChildren :: String -> Maybe String -> IO PageBlocks
retrieveBlockChildren blockToFetch startCursor = do
  -- putStrLn ("Fetching the children of block " ++ blockToFetch)
  initialRequest <- parseRequest $ baseUrl ++ "/blocks/" ++ blockToFetch ++ "/children"
  let request
        = setRequestMethod "GET"
        $ setRequestQueryString (blockChildrenParams startCursor)
        $ addRequestHeader "Notion-Version" apiVersion
        $ setRequestBearerAuth apiKey initialRequest

  response <- httpJSON request
  return $ responseBody response

blockChildrenParams :: Maybe String -> Query
blockChildrenParams Nothing = []
blockChildrenParams (Just startCursor) = [("start_cursor", Just $ S8.pack startCursor)]

retrieveAllBlockChildren :: String -> IO [Block]
retrieveAllBlockChildren bId = do
  first <- retrieveBlockChildren bId Nothing
  retrieveAllBlockChildren' bId (nextCursor first) (pure $ results first)

retrieveAllBlockChildren' :: String -> Maybe String -> IO [Block] -> IO[Block]
retrieveAllBlockChildren' bId (Just startCursor) bks = do
  acm <- bks
  crr <- retrieveBlockChildren bId (Just startCursor)
  retrieveAllBlockChildren' bId (nextCursor crr) (pure $ acm <> results crr)
retrieveAllBlockChildren' _ Nothing bks = bks
