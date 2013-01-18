{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>), (>>^), arr)
import Data.List     (intercalate)
import Data.Time
import System.Locale (defaultTimeLocale)

import Hakyll

-- Set this to False to compile a version of the website with links to
-- all available lecture notes + HWs, not just those in the past.  Be
-- sure to rm -rf web/_cache before rebuilding.
filterLecs = True

main = do
  today <- getCurrentDate

  hakyll $ do
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  matchAny [ "docs/**"
           , "images/**"
           , "hw/**"
           , "extras/**"
           , "lectures/*.lhs"
           ]
    static

  match "templates/*" $ compile templateCompiler

  match "header.markdown" $ compile pageCompiler

  match (list [ "index.markdown"
              , "resources.markdown"
              , "final.markdown"
              ]
        )
    (defaultRules pageCompiler)

  match "lectures/*.html" $
    defaultRules (readPageCompiler >>> addDefaultFields)

  match "lectures/*.markdown" $ compile readPageCompiler

  match "lectures.markdown" $
    defaultRules (pageCompiler >>> addLectures today)

matchAny pats rules = mapM_ (flip match rules) pats

static = route idRoute >> compile copyFileCompiler

defaultRules pc = do
  route $ setExtension "html"
  compile (pc >>>
           setFieldPage "header" "header.markdown" >>>
           applyTemplateCompiler "templates/default.html" >>>
           relativizeUrlsCompiler
          )

addLectures :: (Integer,Int,Int) -> Compiler (Page String) (Page String)
addLectures today = compileLectures today
  >>> arr (\p -> fmap (++ getField "lectures" p) p)

compileLectures :: (Integer,Int,Int) -> Compiler (Page String) (Page String)
compileLectures today =
  requireAllA "lectures/*.markdown" $
  setFieldA "lectures" $
  arr (if filterLecs then filter (before today) else id >>> map compileSources) >>>
  pageListCompiler id "templates/lecture.markdown" >>^
  (readPandoc Markdown Nothing >>> writePandoc)

before :: (Integer,Int,Int) -> Page String -> Bool
before (y,m,d) page =
  case getFieldMaybe "date" page of
    Nothing -> False
    Just dt ->
      case readsTime defaultTimeLocale "%e %B %Y" (dt ++ " " ++ show y) of
        [(day, _)] ->
          let (_,m',d') = toGregorian day
          in  (m',d') <= (m,d)
        _ -> False

compileSources :: Page String -> Page String
compileSources p = setField "sources" sources p
  where
    name    = getField "name" p
    sources = tex ++ extra
    tex     = "[tex](static/" ++ name ++ ".tex)"
    extra   =
      case getFieldMaybe "extra" p of
        Nothing -> ""
        Just es -> (", " ++) . intercalate ", " . map linkify . words $ es
    linkify e = "[" ++ e ++ "](static/" ++ name ++ "/" ++ e ++ ")"

getCurrentDate :: IO (Integer,Int,Int)
getCurrentDate = do
  nowUTC <- getCurrentTime
  tz     <- getCurrentTimeZone
  let mins    = timeZoneMinutes tz * 60
      now     = addUTCTime (fromIntegral mins) nowUTC
      today   = toGregorian . utctDay $ now
  return today
