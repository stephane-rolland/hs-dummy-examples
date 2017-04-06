-- main :: IO ()
-- main = interact interaction

-- interaction :: String -> String
-- interaction x | length x > 10 = "long word" 
-- interaction (x:xs) = xs
-- interaction x = x
-- interaction "ok" = "yes"
-- interaction "ko" = "no"


main :: IO ()
main = interact $ unlines . map interaction . lines

interaction :: String -> String
interaction x | length x > 10 = "long word"



































