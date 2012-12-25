{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Category
import Data.List
import Data.Monoid
import Data.Ord
import Hakyll
import Prelude hiding (id,(.))

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ""
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render posts list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" (id *** arr dateOrdered >>> addPostList)
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . dateOrdered) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Read tempplates
    match "templates/*" $ compile templateCompiler

-- | Sort pages by their date field.
dateOrdered :: [Page a] -> [Page a]
dateOrdered = sortBy (comparing (getField "date"))

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr reverse
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody
