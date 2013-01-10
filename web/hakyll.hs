{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>), (>>^), arr)
import Data.List     (intercalate)
import Hakyll

main = hakyll $ do
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
    defaultRules (pageCompiler >>> addLectures)

matchAny pats rules = mapM_ (flip match rules) pats

static = route idRoute >> compile copyFileCompiler

defaultRules pc = do
  route $ setExtension "html"
  compile (pc >>>
           setFieldPage "header" "header.markdown" >>>
           applyTemplateCompiler "templates/default.html" >>>
           relativizeUrlsCompiler
          )

addLectures :: Compiler (Page String) (Page String)
addLectures = compileLectures
  >>> arr (\p -> fmap (++ getField "lectures" p) p)

compileLectures =
  requireAllA "lectures/*.markdown" $
  setFieldA "lectures" $
  arr (map compileSources) >>>
  pageListCompiler id "templates/lecture.markdown" >>^
  (readPandoc Markdown Nothing >>> writePandoc)

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
