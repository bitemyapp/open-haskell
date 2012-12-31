{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>))
import Hakyll

main = hakyll $ do
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
    
  match "static/*" static
  
  match "templates/*" $ compile templateCompiler

  match "header.markdown" $ compile pageCompiler

  match (list [ "index.markdown"
              , "lectures.markdown"
              , "resources.markdown"
              , "final.markdown"
              ]
        ) 
    (defaultRules pageCompiler)

--  match "lectures.markdown" $ defaultRules (pageCompiler >>> addLectures)
    
  match "lectures/*" $ defaultRules (readPageCompiler >>> addDefaultFields)
  
static = route idRoute >> compile copyFileCompiler
  
defaultRules pc = do
  route $ setExtension "html"
  compile (pc >>>
           setFieldPage "header" "header.markdown" >>>
           applyTemplateCompiler "templates/default.html" >>>
           relativizeUrlsCompiler
          )

{-
addLectures :: Compiler (Page String) (Page String)
addLectures =
  setFieldPageList id
    "templates/lecture.html"
    "projects"
    "projects/*"
  >>> arr (\p -> fmap (++ getField "projects" p) p)
-}