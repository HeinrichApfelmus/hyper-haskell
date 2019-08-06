module Hyper.Extra (
    -- * Synopsis
    -- | Visual representation for various data types.
    
    -- * SVG
    fromSvg,
    -- * Diagrams
    dia,

    -- * QuickCheck
    hyperCheck,
    ) where

import Hyper

import qualified Data.Text        as Text
import qualified Data.Text.Lazy   as Text.Lazy

import           Diagrams.Prelude                   hiding (pre)
import           Diagrams.Backend.SVG
import qualified Graphics.Svg

import qualified Test.QuickCheck  as Q

{-----------------------------------------------------------------------------
    Integration of the `svg-builder` and `diagrams-svg` spackages
------------------------------------------------------------------------------}
-- | Render a SVG document.
-- This assumes that the argument is indeed a complete SVG document,
-- i.e. an @<SVG>@ tag.s
fromSvg :: Graphics.Svg.Element -> Graphic
fromSvg = html . Text.Lazy.toStrict . Graphics.Svg.renderText

-- | Render a diagram via SVG.
dia :: QDiagram SVG V2 Double Any -> Graphic
dia = fromSvg . renderDia SVG (SVGOptions (mkWidth 250) Nothing (Text.pack "") [] True)

{-----------------------------------------------------------------------------
    Integration of the `QuickCheck` package
------------------------------------------------------------------------------}
hyperCheck :: Q.Testable prop => prop -> IO Graphic
hyperCheck =
    fmap (pre . Q.output) . Q.quickCheckWithResult (Q.stdArgs{ Q.chatty = False })

pre s = html . Text.pack $ "<pre>" ++ s ++ "</pre>"
