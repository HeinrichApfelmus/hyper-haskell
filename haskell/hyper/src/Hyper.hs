{-# LANGUAGE FlexibleInstances #-}
module Hyper (
    -- * Synopsis
    -- | Visualizing and representing Haskell values in the HyperHaskell interpreter.
    
    -- * Graphics
    Graphic, string, html,
    
    -- * Display class
    Display(..),
    
    -- * Internal
    displayIO,
    ) where

import Hyper.Internal
