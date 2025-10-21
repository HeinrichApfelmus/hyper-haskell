module Hyper.Extra (
    -- * Synopsis
    -- | Visual representation for various data types.

    -- * SVG
    fromSvg,
    -- * Diagrams
    dia,
    -- * Charts
    chart,

    -- * QuickCheck
    hyperCheck,
    ) where

import Hyper ( Graphic, html )
import System.IO.Unsafe ( unsafePerformIO )

import qualified Data.Text        as Text
import qualified Data.Text.Lazy   as Text.Lazy

import           Diagrams.Prelude                   hiding (pre)
import           Diagrams.Backend.SVG
import qualified Graphics.Svg

import qualified Graphics.Rendering.Chart as Chart

import qualified Test.QuickCheck  as Q
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart

{-----------------------------------------------------------------------------
    Integration of the `svg-builder` and `diagrams-svg` spackages
------------------------------------------------------------------------------}
-- | Render a SVG document.
-- This assumes that the argument is indeed a complete SVG document,
-- i.e. an @<SVG>@ tag.
fromSvg :: Graphics.Svg.Element -> Graphic
fromSvg = html . Text.Lazy.toStrict . Graphics.Svg.renderText

-- | Render a diagram via SVG.
--
-- Uses absolute units:
-- one unit in the diagram corresponds to @1px@ in the rendered image.
--
-- To change the scale in your diagram before rendering, use 'scale'.
dia :: QDiagram SVG V2 Double Any -> Graphic
dia = fromSvg . renderDia SVG (SVGOptions sz Nothing (Text.pack "") [] True)
  where
    sz = mkSizeSpec2D Nothing Nothing

{-----------------------------------------------------------------------------
    Integration of the `Chart` package
------------------------------------------------------------------------------}
chartX, chartY, chartScale :: Double
chartScale = 1.3
chartX = 640 / chartScale
chartY = 480 / chartScale

-- | Create a global environment for rendering charts.
-- Essentially, this will load fonts.
globalChartEnv :: Chart.DEnv Double
globalChartEnv = unsafePerformIO $
    Chart.defaultEnv Chart.vectorAlignmentFns chartX chartY

-- | Render a chart via SVG.
--
-- The rendered image has size 640 x 480 px.
chart :: Chart.ToRenderable a => a -> Graphic
chart = dia
    . scale chartScale
    . fst . Chart.runBackend globalChartEnv
    . flip Chart.render (chartX, chartY) . Chart.toRenderable

{-----------------------------------------------------------------------------
    Integration of the `QuickCheck` package
------------------------------------------------------------------------------}
hyperCheck :: Q.Testable prop => prop -> IO Graphic
hyperCheck =
    fmap (pre . Q.output) . Q.quickCheckWithResult (Q.stdArgs{ Q.chatty = False })

pre s = html . Text.pack $ "<pre>" ++ s ++ "</pre>"
