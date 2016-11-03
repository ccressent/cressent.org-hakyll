{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM, msum)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, parseTimeM, TimeLocale, defaultTimeLocale)
import Hakyll


main :: IO ()
main = hakyllWith config $ do
    match "templates/*" $ compile templateCompiler

    match (fromList staticFiles) $ do
        route   idRoute
        compile copyFileCompiler

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "styles/*" $
        compile $ liftM (fmap compressCss) $
            getResourceFilePath
            >>= \fp -> unixFilter "sass" ["--scss", fp] ""
            >>= makeItem

    create ["styles.css"] $ do
        route idRoute
        compile $ do
            items <- loadAll "styles/*"
            makeItem $ concatMap itemBody (items :: [Item String])

    match "about.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/main.html" defaultContext
            >>= relativizeUrls

    match "home.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/main.html" defaultContext
            >>= relativizeUrls

    match "articles/*" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.html" articleCtx
            >>= loadAndApplyTemplate "templates/main.html" articleCtx
            >>= relativizeUrls

    create ["articles.html"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let context =
                    listField "articles" articleCtx (return articles)
                 <> constField "title" "Articles"
                 <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/article-list.html" context
                >>= loadAndApplyTemplate "templates/main.html" context
                >>= relativizeUrls


staticFiles :: [Identifier]
staticFiles = [ ".htaccess"
              , "404.html"
              , "favicon.ico"
              ]


articleCtx :: Context String
articleCtx =
    dateField "date" "%B %e, %Y"
 <> updatedField "updated" "%B %e, %Y"
 <> defaultContext


config :: Configuration
config = defaultConfiguration
    { providerDirectory = "src/"
    , ignoreFile        = const False
    , deployCommand     = "echo 'No deploy command specified' && exit 1"
    }


-- An "updated" datetime field that can be explicitely put in the document's
-- metadata.
-- Thanks to http://david.sferruzza.fr/posts/2014-06-18-new-blog-with-hakyll.html
updatedField :: String -> String -> Context a
updatedField key format = field key $ \i -> do
    time <- getUpdatedTime locale $ itemIdentifier i
    return $ formatTime locale format time
  where
    locale = defaultTimeLocale

getUpdatedTime :: MonadMetadata m => TimeLocale -> Identifier -> m UTCTime
getUpdatedTime locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = lookupString k metadata >>= parseTime' fmt
    maybe empty' return $ msum [tryField "updated" fmt | fmt <- formats]
  where
    empty' = fail $ "getUpdatedTime: " ++ "could not parse time for " ++
                    show id'
    parseTime' = parseTimeM True locale
    formats =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]
