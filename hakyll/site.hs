--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow   (first)
import           Control.Monad   (ap, (>=>))
import           Data.Monoid     (mappend, (<>))
import           Data.Text       (pack, unpack)
import qualified Data.Text       as T
import           Hakyll
import           System.FilePath (takeBaseName, takeDirectory, takeFileName,
                                  (</>))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "code/*"   $ route idRoute >> compile copyFileCompiler
  match "css/*"    $ route idRoute >> compile compressCssCompiler
  match "fonts/*"  $ route idRoute >> compile copyFileCompiler
  match "images/*" $ route idRoute >> compile copyFileCompiler

  match "404.html" $ do
    route   $ idRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeAndCleanUrls

  match "pages/*.html" $ do
    route   $ customRoute ((</> "index.html") . takeBaseName . toFilePath)
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeAndCleanUrls

  tags <- buildTags postsPattern (fromCapture "tag/*/index.html")

  tagsRules tags $ \tag pattern -> do
    route   $ idRoute
    compile $ do
      tagCtx <- fmap (postCtxWithTitle (postsAbout tag)) . recentFirst
               =<< loadAll pattern
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html"     tagCtx
        >>= loadAndApplyTemplate "templates/default.html" tagCtx
        >>= relativizeAndCleanUrls

  match postsPattern $ do
    route   $ postRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
      >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
      >>= relativizeAndCleanUrls

  create ["posts.html"] $ do
    route   $ cleanRoute
    compile $ do
      archiveCtx <- fmap (postCtxWithTitle "All Posts") . recentFirst
                   =<< loadAll postsPattern
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeAndCleanUrls

  match "index.html" $ do
    route   $ customRoute toFileName
    compile $ do
      indexCtx <- fmap (postCtxWithTitle "Recent Posts" . take 5) . recentFirst
                 =<< loadAll postsPattern
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeAndCleanUrls

  match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%e %B, %Y" <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

postCtxWithTitle :: String -> [Item String] -> Context String
postCtxWithTitle title posts = listField "posts" postCtx (return posts)
                               <> constField "title" title
                               <> defaultContext

postRoute :: Routes
postRoute = customRoute $ (</> "index.html") . yearMonthDirs . toBaseName
  where
    toBaseName    = takeBaseName . toFilePath
    yearMonthDirs = uncurry (</>) .
                    first (map dashToSlash . take 7) . -- length "YYYY/MM"
                    splitAt 11 -- length "YYYY-MM-DD-"

postsAbout :: String -> String
postsAbout = ("Posts about " ++)

postsPattern :: Pattern
postsPattern = "posts/*.html"

toFileName :: Identifier -> FilePath
toFileName = takeFileName . toFilePath

dashToSlash :: Char -> Char
dashToSlash '-' = '/'
dashToSlash c   = c


--------------------------------------------------------------------------------
-- https://www.rohanjain.in/hakyll-clean-urls/
-- with some tweaks
--------------------------------------------------------------------------------

cleanRoute :: Routes
cleanRoute = customRoute (indexPath . toFilePath)

indexPath :: FilePath -> FilePath
indexPath p = takeDirectory p </> takeBaseName p </> "index.html"

relativizeAndCleanUrls :: Item String -> Compiler (Item String)
relativizeAndCleanUrls = relativizeUrls >=> cleanIndexUrls -- >=> cleanIndexHtmls

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

-- cleanIndexHtmls :: Item String -> Compiler (Item String)
-- cleanIndexHtmls = return . fmap (replaceAll "/index.html" (const "/"))

cleanIndex :: String -> String
cleanIndex = flip maybe unpack `ap` (T.stripSuffix "index.html" . pack)
