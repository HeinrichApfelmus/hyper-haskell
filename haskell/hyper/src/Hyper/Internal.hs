{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Hyper.Internal (
    -- * Synopsis
    -- | Internal data types used by the HyperHaskell back-end
    -- to analyze values constructed with the 'Hyper' module.
    
    -- * Documentation
    Graphic(..), string, html,
    Display(..),
    displayIO,
    ) where

import           Control.DeepSeq
import           Data.List            (isPrefixOf)
import           Data.Typeable

import qualified Data.Text       as T
import qualified Data.Text.Lazy  as TL
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H

{-----------------------------------------------------------------------------
    Graphics
------------------------------------------------------------------------------}
-- | A graphical representation of data.
data Graphic = Graphic { gHtml :: T.Text } deriving (Typeable)

instance NFData Graphic where rnf g = rnf (gHtml g)

-- | Render a 'String' as a 'Graphic'.
string :: String -> Graphic
string = Graphic . TL.toStrict . H.renderHtml . H.toHtml

-- | Render arbitrary HTML code as a 'Graphic'.
-- 
-- NOTE: This function does not do check whether the input is well-formed HTML.
--
-- NOTE: This function will probably deprecated once we figure out
-- how to do this properly, but for now, just use it.
html :: T.Text -> Graphic
html = Graphic

{-----------------------------------------------------------------------------
    Display class
------------------------------------------------------------------------------}
-- | Class for displaying Haskell values.
class Display a where
    display :: a -> Graphic

instance Display ()           where display x = x `seq` fromShow x
instance Display Graphic      where display = id
instance Display Bool         where display = fromShow
instance Display Double       where display = fromShow
instance Display Integer      where display = fromShow
instance Display Int          where display = fromShow
instance Display String       where display = fromShow
instance Display [Int]        where display = displayList
instance Display [String]     where display = displayList

fromShow :: Show a => a -> Graphic
fromShow = string . show

displayList :: Show a => [a] -> Graphic
displayList = fromShow

{-----------------------------------------------------------------------------
    Internal Class for displaying either IO actions or values
------------------------------------------------------------------------------}
class DisplayIO a where
    displayIO :: a -> IO Graphic

instance Display a => DisplayIO (IO a) where
    displayIO = fmap display
instance Display a => DisplayIO a where
    displayIO = return . display

