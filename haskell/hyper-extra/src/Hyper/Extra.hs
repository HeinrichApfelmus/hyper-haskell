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
-- i.e. an @<SVG>@ tag.s
fromSvg :: Graphics.Svg.Element -> Graphic
fromSvg = html . Text.Lazy.toStrict . Graphics.Svg.renderText

-- | Render a diagram via SVG.
dia :: QDiagram SVG V2 Double Any -> Graphic
dia = fromSvg . renderDia SVG (SVGOptions (mkWidth 250) Nothing (Text.pack "") [] True)

{-----------------------------------------------------------------------------
    Integration of the `Chart` package
------------------------------------------------------------------------------}
-- | Create a global environment for rendering charts.
-- Essentially, this will load fonts.
globalChartEnv :: Chart.DEnv Double
globalChartEnv =
    unsafePerformIO $ Chart.defaultEnv Chart.vectorAlignmentFns 640 480

-- | Render a chart via SVG.
chart :: Chart.ToRenderable a => a -> Graphic
chart = dia
    . fst . Chart.runBackend globalChartEnv
    . flip Chart.render (640, 480) . Chart.toRenderable

{-----------------------------------------------------------------------------
    Integration of the `QuickCheck` package
------------------------------------------------------------------------------}
hyperCheck :: Q.Testable prop => prop -> IO Graphic
hyperCheck =
    fmap (pre . Q.output) . Q.quickCheckWithResult (Q.stdArgs{ Q.chatty = False })

pre s = html . Text.pack $ "<pre>" ++ s ++ "</pre>"
