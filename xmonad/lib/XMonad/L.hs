{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-- Note: This is a temporary place for experimenting with different layouts,
-- so please do not be bothered with the lack of a copyright notice.

module XMonad.L (
  Circular (..),
  limit,
  named,
  reflectVert,
  reflectHoriz) where

import Data.List
import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import XMonad

import qualified XMonad.StackSet as W
import XMonad.Core (X, LayoutClass(..), fromMessage, io, withDisplay)
import XMonad.Layout (Resize(..), IncMasterN(..), tile)
import Graphics.X11.Xlib (Window, rect_width)
import Graphics.X11.Xlib.Extras ( getWMNormalHints
                                , getWindowAttributes
                                , sh_base_size
                                , sh_resize_inc
                                , wa_border_width)

import qualified XMonad.Layout.Renamed as Layout.Renamed
import qualified XMonad.Layout.LimitWindows as Layout.LimitWindows
import qualified XMonad.Layout.Reflect as Layout.Reflect

------------------------------------------------------------
named s layout = Layout.Renamed.renamed [Layout.Renamed.Replace s] layout
limit n layout = Layout.LimitWindows.limitWindows n layout
reflectHoriz layout = Layout.Reflect.reflectHoriz layout
reflectVert layout = Layout.Reflect.reflectVert layout

------------------------------------------------------------
data Circular a = Circular deriving (Read, Show)

instance LayoutClass Circular Window where
  doLayout Circular r s =
    do layout <- raiseFocus (circularLayout r (W.integrate s))
       return (layout, Nothing)

circularLayout :: Rectangle -> [a] -> [(a, Rectangle)]
circularLayout _ [] = []
circularLayout r (w:ws) = master : rest
  where master = (w, center r)
        rest = zip ws (map
                       (satellite r)
                       [0, pi * 2 / fromIntegral (length ws) ..])

raiseFocus :: [(Window, Rectangle)] -> X [(Window, Rectangle)]
raiseFocus xs = do focused <- withWindowSet (return . W.peek)
                   return $ case find ((== focused) . Just . fst) xs of
                              Just x  -> x : delete x xs
                              Nothing -> xs

center :: Rectangle -> Rectangle
center (Rectangle sx sy sw sh) = Rectangle x y w h
    where s1 = 1.4
          s2 = 1.05
          w = round (fromIntegral sw / s1)
          h = round (fromIntegral sh / s2)
          x = sx + fromIntegral (sw - w) `div` 2
          y = sy + fromIntegral (sh - h) `div` 2

satellite :: Rectangle -> Double -> Rectangle
satellite (Rectangle sx sy sw sh) a =
  Rectangle (sx + round (rx + rx * cos a))
  (sy + round (ry + ry * sin a))
  w h
  where rx = fromIntegral (sw - w) / 2
        ry = fromIntegral (sh - h) / 2
        w = sw * 10 `div` 25
        h = sh * 10 `div` 25
