{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Feed
import           Hakyll.Web.Tags
import qualified Data.Set as S
import           Text.Pandoc.Options


feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration { feedTitle = "austinschwartz.com"
                               , feedDescription = "Austin Schwartz"
                               , feedAuthorName = "Austin Schwartz"
                               , feedRoot = "http://www.austinschwartz.com"
                               , feedAuthorEmail = "schwar12@purdue.edu"
                               }

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                writerExtensions = newExtensions,
                writerHTMLMathMethod = MathJax ""
                }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

main :: IO ()
main = hakyll $ do
    match "assets/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "assets/css/*.scss" $ do
        route   $ setExtension "css"
        compile $ getResourceString 
          >>= withItemBody (unixFilter "sass" ["-s", "--scss"]) 
          >>= return . fmap compressCss

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "partials/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*.md"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "partials/post-list.html" archiveCtx
                >>= loadAndApplyTemplate "partials/index.html" archiveCtx
                >>= loadAndApplyTemplate "templates/blank.html" archiveCtx
                >>= relativizeUrls

    create ["posts/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*.md"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "partials/post-list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "partials/*"  $ compile templateCompiler
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%m/%d/%y" `mappend`
    dateField "dateLong" "%B %e, %Y" `mappend`
    defaultContext

homeCtx :: Context String
homeCtx =
    constField "title" "Home" `mappend`
    postCtx
