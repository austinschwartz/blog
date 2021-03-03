{-# LANGUAGE OverloadedStrings #-}
{-Some of this taken from https://github.com/zacharydenton/zach.se-}
import Control.Applicative
import Text.Pandoc.Options
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Data.Char
import System.Process (readProcess)
import System.IO.Unsafe
import Hakyll

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration {
    feedTitle = "Austin Schwartz",
    feedDescription = "Austin Schwartz",
    feedAuthorName = "Austin Schwartz",
    feedRoot = "https://austinschwartz.com",
    feedAuthorEmail = "me@austinschwartz.com"
}
 {-modified from bitbucket.org/honk/pandoc-filters-}
{-pygmentize :: Pandoc -> Compiler Pandoc-}
{-pygmentize (Pandoc meta bs) = Pandoc meta <$> mapM highlight bs-}

{-highlight :: Block -> Compiler Block-}
{-highlight (CodeBlock (_, options, _) code) =-}
  {-RawBlock "html" <$> unsafeCompiler (pygments code options)-}
{-highlight x = return x-}

{-pygments :: String -> [String] -> IO String-}
{-pygments code options =-}
  {-case options of-}
    {-(lang:_) ->-}
      {-readProcess "pygmentize" ["-l", toLower <$> lang,  "-f", "html"] code-}
    {-_ -> return $ "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"-}

compiler :: Compiler (Item String)
compiler =
    let writerOptions = defaultHakyllWriterOptions {
                          writerHTMLMathMethod = KaTeX ""
                        }
      in pandocCompilerWith defaultHakyllReaderOptions
                                      writerOptions
                                      {-pygmentize-}

renderKaTeX :: Item String -> Compiler (Item String)
renderKaTeX = withItemBody (unixFilter "bin/katex.js" [])

main :: IO ()
main = hakyllWith config $ do
    match "static/**" $ do
        route   setRoot
        compile copyFileCompiler

    match "assets/**.scss" $ do
        route $ setRoot `composeRoutes` setExtension "css"
        compile $ getResourceString
          >>= withItemBody (unixFilter "sassc" ["-s"])
          >>= withItemBody (unixFilter "uglifycss" [])

    match "assets/**.css" $ do
        route $ setRoot `composeRoutes` setExtension "css"
        compile $ compressCssCompiler

    match "assets/**.js" $ do
        route $ setRoot `composeRoutes` setExtension "js"
        compile $ getResourceString
            >>= withItemBody
                (unixFilter "uglifyjs" [])

    match "pages/**" $ do
        route   $ setRoot `composeRoutes` cleanURL
        compile $ compiler
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls

    categories <- buildCategories allPostsPattern (fromCapture "categories/*.html")

    match allPostsPattern $ do
        route $ postRoute `composeRoutes` cleanURL
        compile $ compiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts.html"] $ do
        route cleanURL
        compile $ do
            posts <- recentFirst =<< loadAll postPattern
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postPattern
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "blog" "Posts" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    {-create ["index.html"] $ do-}
        {-route idRoute-}
        {-compile $ do-}
            {-posts <- recentFirst =<< loadAll postPattern-}
            {-let archiveCtx = -}
                    {-listField "posts" postCtx (return posts) `mappend`-}
                    {-constField "title" "Posts" `mappend`-}
                    {-defaultContext-}
            {-makeItem ""-}
                {->>= loadAndApplyTemplate "pages/index.html" archiveCtx-}
                {->>= loadAndApplyTemplate "templates/default.html" archiveCtx-}
                {->>= relativizeUrls-}

    create ["rss.xml"] $ do
        route idRoute
        compile $
            loadAll postPattern
                >>= fmap (take 10) . recentFirst
                >>= renderAtom feedConfiguration feedCtx

    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%m/%d/%y" `mappend`
    dateField "dateLong" "%B %e, %Y" `mappend`
    field "url" (fmap (maybe empty (dropFileName . toUrl)) . getRoute . itemIdentifier) `mappend`
    boolField "comments" (const True) `mappend`
    defaultContext

pageCtx :: Context String
pageCtx =
    boolField "comments" (const False) `mappend`
    defaultContext

feedCtx :: Context String
feedCtx =
    bodyField "description" `mappend`
    postCtx

postRoute :: Routes
postRoute = customRoute $ removeDate . basename

postPattern :: Pattern
postPattern = "posts/*"

allPostsPattern :: Pattern
allPostsPattern = "posts/**"

setRoot :: Routes
setRoot = customRoute stripTopDir

removeDate :: FilePath -> FilePath
removeDate = drop 11

stripTopDir :: Identifier -> FilePath
stripTopDir = joinPath . tail . splitPath . toFilePath

basename :: Identifier -> FilePath
basename = last . splitPath . toFilePath

cleanURL :: Routes
cleanURL = customRoute fileToDirectory

fileToDirectory :: Identifier -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

config :: Configuration
config = defaultConfiguration {
        deployCommand = "bin/deploy.sh"
    }
