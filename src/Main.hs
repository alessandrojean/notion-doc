module Main (main) where

import NotionApi
import Converter
import Formats

tutorialPageId :: String
tutorialPageId = "7b85035517424c72ab58af014fecf6bc"

main :: IO ()
main = do
  page <- retrieveNotionPage tutorialPageId
  -- print page

  blocks <- retrieveAllBlockChildren tutorialPageId
  -- print blocks

  let ast = createAst page blocks

  saveMarkdown "page.md" ast

