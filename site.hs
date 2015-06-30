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

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.md" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/index.html" postCtx
            >>= relativizeUrls

    match "partials/*"  $ compile templateCompiler
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
