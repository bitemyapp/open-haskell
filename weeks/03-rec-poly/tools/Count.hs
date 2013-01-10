import Data.Char

main :: IO ()
main = interact ((++"\n") . show . length . filter (not . isSpace))