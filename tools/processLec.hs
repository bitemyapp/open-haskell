{-# LANGUAGE DeriveDataTypeable #-}

-- CIS 194, F'10, S'12, S'13.
-- Utility to prepare in-class versions and HTML versions of .lhs files:
--   + strip HTML comments
--   + consecutively number examples
--   + add compile date+time
--
-- See http://johnmacfarlane.net/pandoc/scripting.html

import System.Console.CmdArgs

import Text.Pandoc hiding (Target)
import Data.List (isPrefixOf)
import Text.Printf
import Data.Time

import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad.Supply

import Data.Char

------------------------------------------------------------
-- Command-line arguments.
------------------------------------------------------------

data Target = Class | Html | Slides
  deriving (Show, Data, Typeable, Eq)

------------------------------------------------------------
-- Process lecture notes
------------------------------------------------------------

-- Strip comments not intended for the current target.
stripComments :: Target -> [Block] -> [Block]
stripComments targ = concatMap stripComments'
  where
    stripComments' :: Block -> [Block]
    stripComments' (Plain [RawInline "html" s]) = doStrip s
    stripComments' (RawBlock "html" s)          = doStrip s
    stripComments' b = [b]

    doStrip s
      | "<!--" `isPrefixOf` s && forTarget = inc
      | otherwise = [RawBlock "html" s]
      where (Pandoc _ inc) = readLHS . unlines . init . tail . lines $ s
            forTarget      = upper (show targ) `elem`
                             (map upper . words . head . lines . drop 4 $ s)
    upper = map toUpper

-- Number examples consecutively, using the Supply monad to provide
-- consecutive numbers as needed.
numberExamples :: Pandoc -> Pandoc
numberExamples = flip evalSupply [1..] . bottomUpM numberBlock
  where numberBlock :: Block -> Supply Int Block
        numberBlock (CodeBlock attr str)
          = CodeBlock attr . unlines <$> mapM numberLine (lines str)
        numberBlock b = return b

        numberLine :: String -> Supply Int String
        numberLine s
          | "ex" `isPrefixOf` s && (s !! 2 `notElem` ['a'..'z']) = do
              n <- supply
              return $ "ex" ++ printf "%02d" n ++ " " ++ dropWhile (/= '=') s
          | otherwise = return s

addTimestamp :: String -> Pandoc -> Pandoc
addTimestamp time (Pandoc meta blocks) = Pandoc meta (blocks ++ [HorizontalRule, genMsg])
  where genMsg = Para [Code nullAttr $ "Generated " ++ time]

transformDoc :: Target -> String -> Pandoc -> Pandoc
transformDoc targ time = bottomUp (stripComments targ)
                       . numberExamples
                       . addTimestamp time

------------------------------------------------------------
-- Slide generation
------------------------------------------------------------

{-
data SlideItem = Frame { frameTitle   :: String
                       , frameContent :: Pandoc
                       }
               | Section String
               | Subsection String

type Deck = [SlideItem]
-}

-- Extract all the code blocks
data Code = Hask String
          | Verb String

genSlides :: Pandoc -> [Code]
genSlides = queryWith getCode
  where getCode (CodeBlock (_,ts,_) c)
          | "haskell" `elem` ts = [Hask c]
          | otherwise           = [Verb c]
        getCode _               = []

writeSlides :: [Code] -> String
writeSlides = unlines . map mkFrame
  where mkFrame (Hask c) = env "frame" . env "code" $ (c ++ "\n")
        mkFrame (Verb c) = env' "frame" (Just "fragile") . env "verbatim" $ (c ++ "\n")

env :: String -> String -> String
env s = env' s Nothing

env' :: String -> Maybe String -> String -> String
env' e optArg body = "\\begin{" ++ e ++ "}" ++ arg ++ "\n" ++ body ++ "\\end{" ++ e ++ "}\n"
  where arg = case optArg of { Nothing -> "" ; Just o -> "[" ++ o ++ "]" }

------------------------------------------------------------
-- General format reading/writing
------------------------------------------------------------

readLHS :: String -> Pandoc
readLHS = readMarkdown defaultParserState { stateLiterateHaskell = True
                                          , stateSmart           = True
                                          }

writeLHS :: Pandoc -> String
writeLHS = writeMarkdown defaultWriterOptions { writerLiterateHaskell = True }

writeHTML :: Pandoc -> String
writeHTML = writeHtmlString defaultWriterOptions { writerLiterateHaskell = True }

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = do
  targ    <- cmdArgs (modes [ Class  &= help "Output in-class .lhs"
                            , Html   &= help "Output .html"
                            , Slides &= help "Output slides"
                            ])
  utcTime <- getCurrentTime
  tz      <- getCurrentTimeZone
  let time = show $ utcToLocalTime tz utcTime
  interact (chooseTransform time targ)

chooseTransform time Class  = writeLHS  . transformDoc Class time . readLHS
chooseTransform time Html   = writeHTML . transformDoc Html  time . readLHS
chooseTransform _    Slides = writeSlides . genSlides . readLHS
