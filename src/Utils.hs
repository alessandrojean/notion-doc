module Utils (dropLast, fixBlockType) where

dropLast :: Int -> [a] -> [a]
dropLast _ [] = []
dropLast n xs = take (length xs - n) xs

fixBlockType :: String -> String
fixBlockType "Heading1" = "Heading_1"
fixBlockType "heading1" = "heading_1"
fixBlockType "Heading2" = "Heading_2"
fixBlockType "heading2" = "heading_2"
fixBlockType "Heading3" = "Heading_3"
fixBlockType "heading3" = "heading_3"
fixBlockType s = s
