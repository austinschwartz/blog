{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.Process
import           Text.Pandoc
import           Hakyll.Web.Feed
import           Hakyll.Web.Tags
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Char
import           Text.Pandoc.Options


feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration { feedTitle = "austinschwartz.com"
                               , feedDescription = "Austin Schwartz"
                               , feedAuthorName = "Austin Schwartz"
                               , feedRoot = "http://www.austinschwartz.com"
                               , feedAuthorEmail = "me@austinschwartz.com"
                               }

mathCtx :: Context a
mathCtx = field "katex" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if "katex" `M.member` metadata
			then "<link rel=\"stylesheet\" href=\"/css/katex.min.css\">\n\
								\<script type=\"text/javascript\" src=\"/js/katex.min.js\"></script>\n\
								\<script src=\"/js/auto-render.min.js\"></script>"
			else ""

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

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  let extraExtensions =
        [ Ext_east_asian_line_breaks
        , Ext_tex_math_double_backslash
        ]
      customExtensions = foldr S.insert pandocExtensions extraExtensions
      writerOptions = defaultHakyllWriterOptions {
          writerExtensions = customExtensions
        , writerHighlight = True
        , writerHTMLMathMethod = MathJax ""
        }
  in   pandocCompilerWithTransformM defaultHakyllReaderOptions
                                    writerOptions
                                    pygmentize

-- modified from bitbucket.org/honk/pandoc-filters
pygmentize :: Pandoc -> Compiler Pandoc
pygmentize (Pandoc meta bs) = Pandoc meta <$> mapM highlight bs

highlight :: Block -> Compiler Block
highlight (CodeBlock (_, options, _) code) =
  RawBlock "html" <$> unsafeCompiler (pygments code options)
highlight x = return x

pygments :: String -> [String] -> IO String
pygments code options =
  case options of
    (lang:_) ->
      readProcess "pygmentize" ["-l", toLower <$> lang,  "-f", "html"] code
    _ -> return $ "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"

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
          >>= withItemBody (unixFilter "sassc" ["-s"]) 
          >>= return . fmap compressCss

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll pattern
          let ctx = constField "title" title 
                    `mappend` listField "posts" postCtx (return posts) 
                    `mappend` defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "partials/tag.html" ctx
              >>= loadAndApplyTemplate "templates/blank.html" ctx
              >>= relativizeUrls

    noteTags <- buildTags "notes/*" (fromCapture "tags/*.html")

    tagsRules noteTags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll pattern
          let ctx = constField "title" title 
                    `mappend` listField "posts" postCtx (return posts) 
                    `mappend` defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "partials/tag.html" ctx
              >>= loadAndApplyTemplate "templates/blank.html" ctx
              >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "partials/post.html"   (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/blank.html" (postCtxWithTags tags)
            >>= relativizeUrls

    match "posts-draft/*" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "partials/post.html"   (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/blank.html" (postCtxWithTags tags)
            >>= relativizeUrls

    match "notes/*" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "partials/post.html"   (postCtxWithTags noteTags)
            >>= loadAndApplyTemplate "templates/blank.html" (postCtxWithTags noteTags)
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
                >>= loadAndApplyTemplate "partials/index2.html" archiveCtx
                >>= loadAndApplyTemplate "templates/blank.html" archiveCtx
                >>= relativizeUrls

    create ["books/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*.md"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"            `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "partials/books.html" archiveCtx
                >>= loadAndApplyTemplate "templates/blank.html" archiveCtx
                >>= relativizeUrls


    create ["about/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*.md"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"            `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "partials/about.html" archiveCtx
                >>= loadAndApplyTemplate "templates/blank.html" archiveCtx
                >>= relativizeUrls

    {-match "books.html" $ do-}
        {-route $ gsubRoute "books.html" (const "books/index.html") -}
        {-compile $ pandocMathCompiler-}
            {->>= loadAndApplyTemplate "templates/blank.html" postCtx-}
            {->>= relativizeUrls-}

    create ["posts/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*.md"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"            `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "partials/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/blank.html" archiveCtx
                >>= relativizeUrls

    create ["posts-draft/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts-draft/*.md"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Draft Posts"            `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "partials/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/blank.html" archiveCtx
                >>= relativizeUrls

    create ["notes/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "notes/*.md"
            let archiveCtx =
                    listField "notes" postCtx (return posts) `mappend`
                    constField "title" "Notes"            `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "partials/notes.html" archiveCtx
                >>= loadAndApplyTemplate "templates/blank.html" archiveCtx
                >>= relativizeUrls

    match "partials/*"  $ compile templateCompiler
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%m/%d/%y" `mappend`
    dateField "dateLong" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

{-noteCtx :: Context String-}
{-noteCtx =-}
    {-dateField "date" "%m/%d/%y" `mappend`-}
    {-dateField "dateLong" "%B %e, %Y" `mappend`-}
    {-defaultContext-}

homeCtx :: Context String
homeCtx =
    constField "title" "Home" `mappend`
    postCtx
