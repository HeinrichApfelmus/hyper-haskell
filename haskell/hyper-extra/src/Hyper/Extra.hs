module Hyper.Extra (
    -- * Synopsis
    -- | Visual representation for various data types.
    
    -- * Diagrams
    dia,

    -- * QuickCheck
    hyperCheck,
    ) where

import Hyper

import qualified Data.Text        as T
import qualified Data.Text.Lazy   as TL

import Diagrams.Prelude                   hiding (pre)
import Diagrams.Backend.SVG
import Graphics.Svg               as SVG

import qualified Test.QuickCheck  as Q

{-----------------------------------------------------------------------------
    Integration of the `diagrams-svg` and `svg-builder` packages
------------------------------------------------------------------------------}
dia :: QDiagram SVG V2 Double Any -> Graphic
dia = html . TL.toStrict . SVG.renderText
    . renderDia SVG (SVGOptions (mkWidth 250) Nothing (T.pack "") [] True)

{-----------------------------------------------------------------------------
    Integration of the `QuickCheck` package
------------------------------------------------------------------------------}
hyperCheck :: Q.Testable prop => prop -> IO Graphic
hyperCheck =
    fmap (pre . Q.output) . Q.quickCheckWithResult (Q.stdArgs{ Q.chatty = False })

pre s = html . T.pack $ "<pre>" ++ s ++ "</pre>"
