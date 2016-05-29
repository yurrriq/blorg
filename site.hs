--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow   (first)
import           Data.List       (isSuffixOf)
import           Data.List.Split (splitOn)
import           Data.Monoid     (mappend)
import           Hakyll
import           System.FilePath (replaceExtension, takeBaseName, takeDirectory,
                                  takeFileName, (</>))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["org/about.org", "org/404.org"]) $ do
        route   $ customRoute ((</> "index.html")
                               . takeBaseName
                               . toFilePath)
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    -- match "posts/*.org" $ do
    --     route   $ setExtension "html"
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/post.html"    postCtx
    --         >>= loadAndApplyTemplate "templates/default.html" postCtx
    --         >>= relativizeUrls

    match "posts/*.html" $ do
      route   $ customRoute ((</> "index.html")
                             . uncurry (</>)
                             . first (map dashToSlash . take 7)
                             . splitAt 11
                             . takeBaseName
                             . toFilePath)
      -- route   $ cleanRoute
      compile $ getResourceBody
          -- >>= applyAsTemplate pageCtx
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

    create ["archive.html"] $ do
        route   $ cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archive"             `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls


    match "pages/index.html" $ do
        route   $ customRoute toFileName
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Recent Posts"        `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%e %B, %Y" `mappend`
    defaultContext

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

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern     = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url | idx `isSuffixOf` url = take (length url - length idx) url
               | otherwise            = url
  where idx = "index.html"
