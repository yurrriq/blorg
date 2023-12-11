module Main (main) where

import Control.Arrow (first)
import Control.Monad (ap, (>=>))
import Data.Text (pack, unpack)
import qualified Data.Text as T
import Hakyll
import System.FilePath
  ( replaceExtension,
    takeBaseName,
    takeDirectory,
    takeFileName,
    (</>),
  )

-- ------------------------------------------------------------------ [ Config ]
-- https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html
-- -----------------------------------------------------------------------------

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main = hakyllWith config $ do
  handleStaticFiles
  handleTemplates
  tags <- buildTags postsPattern (fromCapture "tag/*/index.html")
  handleTags tags
  handleStaticPages tags
  handlePosts tags
  handleFeeds tags

-- ------------------------------------------------------------------- [ Rules ]

handleStaticFiles :: Rules ()
handleStaticFiles =
  do
    match "code/*" $
      route idRoute >> compile copyFileCompiler
    match "css/*" $
      route idRoute >> compile compressCssCompiler
    match "fonts/*" $
      route idRoute >> compile copyFileCompiler
    -- match "mp3/*" $
    --   route idRoute >> compile copyFileCompiler
    match "images/*" $
      route idRoute >> compile copyFileCompiler

handleTemplates :: Rules ()
handleTemplates =
  match "templates/*" $
    compile templateCompiler

handleTags :: Tags -> Rules ()
handleTags tags =
  do
    tagsRules tags $ \tag tagPattern -> do
      route idRoute
      compile $ do
        tagCtx <-
          fmap (postCtxWithTitle (postsTagged tag) tags) . recentFirst
            =<< loadAll tagPattern
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" tagCtx
          >>= loadAndApplyTemplate "templates/default.html" tagCtx
          >>= relativizeAndCleanUrls

handleStaticPages :: Tags -> Rules ()
handleStaticPages tags =
  do
    match "404.html" $ do
      route idRoute
      compile $
        getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeAndCleanUrls

    match "index.html" $ do
      route $ customRoute toFileName
      compile $ do
        indexCtx <-
          fmap (postCtxWithTitle "Recent Posts" tags . take 5) . recentFirst
            =<< loadAll postsPattern
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeAndCleanUrls

    create ["posts.html"] $ do
      route cleanRoute
      compile $ do
        archiveCtx <-
          fmap (postCtxWithTitle "All Posts" tags) . recentFirst
            =<< loadAll postsPattern
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeAndCleanUrls

    match "pages/*.html" $ do
      route $ customRoute ((</> "index.html") . takeBaseName . toFilePath)
      compile $
        getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeAndCleanUrls

handlePosts :: Tags -> Rules ()
handlePosts tags =
  match postsPattern $ do
    route postRoute
    compile $
      getResourceBody
        >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
        >>= relativizeAndCleanUrls

handleFeeds :: Tags -> Rules ()
handleFeeds tags =
  do
    let feedCtx = postCtx tags <> bodyField "description"
    create ["index.atom"] $ do
      route idRoute
      compile $
        loadAllSnapshots postsPattern "content"
          >>= fmap (take 10) . recentFirst
          >>= renderAtom feedConfig feedCtx
    tagsRules tags $ \tag tagPattern -> version "atom" $ do
      route $ customRoute (atomPath . toFilePath)
      compile $
        loadAllSnapshots tagPattern "content"
          >>= fmap (take 10) . recentFirst
          >>= renderAtom (tagFeedConfig tag) feedCtx

-- --------------------------------------------------- [ Post helper functions ]

postCtx :: Tags -> Context String
postCtx tags =
  dateField "date" "%e %B, %Y"
    <> tagsField "tags" tags
    <> defaultContext

postCtxWithTitle :: String -> Tags -> [Item String] -> Context String
postCtxWithTitle title tags posts =
  listField "posts" (postCtx tags) (return posts)
    <> constField "title" title
    <> defaultContext

postRoute :: Routes
postRoute = customRoute $ (</> "index.html") . yearMonthDirs . toBaseName
  where
    toBaseName = takeBaseName . toFilePath
    yearMonthDirs =
      uncurry (</>)
        . first (map dashToSlash . take (T.length "YYYY/MM"))
        . splitAt (T.length "YYYY-MM-DD-")

postsTagged :: String -> String
postsTagged tag = "Posts tagged \"" ++ tag ++ "\""

postsPattern :: Pattern
postsPattern = "posts/*.html"

toFileName :: Identifier -> FilePath
toFileName = takeFileName . toFilePath

dashToSlash :: Char -> Char
dashToSlash '-' = '/'
dashToSlash c = c

-- ------------------------------------------------------------ [ Clean routes ]
-- Modified from https://www.rohanjain.in/hakyll-clean-urls/
-- -----------------------------------------------------------------------------

cleanRoute :: Routes
cleanRoute = customRoute (indexPath . toFilePath)

indexPath :: FilePath -> FilePath
indexPath p = takeDirectory p </> takeBaseName p </> "index.html"

relativizeAndCleanUrls :: Item String -> Compiler (Item String)
relativizeAndCleanUrls = relativizeUrls >=> cleanIndexUrls -- >=> cleanIndexHtmls

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndex :: String -> String
cleanIndex = flip maybe unpack `ap` (T.stripSuffix "index.html" . pack)

-- cleanIndexHtmls :: Item String -> Compiler (Item String)
-- cleanIndexHtmls = return . fmap (replaceAll "/index.html" (const "/"))

-- ------------------------------------------------------------------ [ Config ]

feedConfig :: FeedConfiguration
feedConfig =
  FeedConfiguration
    { feedTitle = "blorg.ericb.me",
      feedDescription = "Varyingly coherent ramblings of yet another Lisp hacker.",
      feedAuthorName = "Eric Bailey",
      feedAuthorEmail = "eric@ericb.me",
      feedRoot = "https://blorg.ericb.me"
    }

-- TODO: lenses?
tagFeedConfig :: String -> FeedConfiguration
tagFeedConfig tag =
  feedConfig
    { feedTitle = "blorg.ericb.me - " ++ tag,
      feedDescription = postsTagged tag
    }

atomPath :: FilePath -> FilePath
atomPath = flip replaceExtension ".atom" . takeDirectory

-- --------------------------------------------------------------------- [ EOF ]
