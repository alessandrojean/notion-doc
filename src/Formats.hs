module Formats where

import Text.Pandoc
import qualified Data.Text as T

mdOptions :: WriterOptions
mdOptions = def { writerExtensions = githubMarkdownExtensions }

saveMarkdown :: String -> Pandoc -> IO ()
saveMarkdown fileName doc = do
  result <- runIO $ writeMarkdown mdOptions doc
  md <- handleError result
  writeFile fileName $ T.unpack md
