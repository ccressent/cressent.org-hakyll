{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Hakyll


main :: IO ()
main = hakyllWith config $ do
    match "templates/*" $ compile templateCompiler

    match (fromList staticFiles) $ do
        route   idRoute
        compile copyFileCompiler

    match "styles/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.md" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/main.html" defaultContext
            >>= relativizeUrls

    match "home.md" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
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
              , "fonts/*"
              , "images/*"
              , "favicon.ico"
              ]


articleCtx :: Context String
articleCtx =
    dateField "date" "%B %e, %Y"
 <> defaultContext


config :: Configuration
config = defaultConfiguration
    { providerDirectory = "src/"
    , ignoreFile        = \_ -> False
    , deployCommand     = "echo 'No deploy command specified' && exit 1"
    }
