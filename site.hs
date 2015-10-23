{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Data.List           (sortBy)
import qualified Data.Map            as M
import           Data.Monoid         (mappend)
import           Data.Ord            (comparing)
import qualified Data.Set            as S
import           Hakyll
import           Hakyll.Web.Pandoc
import           Text.Pandoc.Options

getWeek :: MonadMetadata m => Identifier -> m Int
getWeek id' = do
  metadata <- getMetadata id'
  let s = M.lookup "week" metadata
  case s of
    Nothing -> fail "Need a week field in the post!"
    Just s' -> return $ read s'

-- | Sort pages chronologically. Uses the same method as 'dateField' for
-- extracting the date.
-- chronological :: MonadMetadata m => [Item a] -> m [Item a]
-- chronological =
--     sortByM $ getItemUTC defaultTimeLocale . itemIdentifier
--   where

-- Sort by week

weekly :: MonadMetadata m => [Item a] -> m [Item a]
weekly =
  sortByM $ getWeek . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = (map fst . sortBy (comparing snd))
                   <$> mapM (\x -> fmap (x,) (f x)) xs

-- readerOptions :: ReaderOptions
-- readerOptions =
--   updated
--   where
--     exts = readerExtensions defaultHakyllReaderOptions
--     added = S.insert Ext_literate_haskell exts
--     updated = defaultHakyllReaderOptions { readerExtensions = added }

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler -- With readerOptions defaultHakyllWriterOptions
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "lectures/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["lectures.html"] $ do
        route idRoute
        compile $ do
            posts <- weekly =<< loadAll "lectures/*"
            let archiveCtx =
                    listField "lectures" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            weeks <- weekly =<< loadAll "lectures/*"
            let indexCtx =
                    listField "lectures" postCtx (return weeks) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
