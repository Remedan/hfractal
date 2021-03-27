-- Copyright (C) 2021  Vojtěch Balák
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module Main where

import Data.List
import Data.Word

import Data.ByteString
import Data.Complex
import Data.Colour
import Data.Colour.RGBSpace
import qualified Data.Colour.RGBSpace.HSL as HSL
import Graphics.Gloss

stripes :: Int -> Int -> Int -> Int -> RGB Float
stripes w h x y = HSL.hsl (fromIntegral ((x + y) `mod` 360)) 1 0.5

pixelToComplex :: Int -> Int -> Int -> Int -> Complex Float
pixelToComplex w h x y =
    let rMin = -2
        rMax = 1
        iMin = -1
        iMax = 1
        transform a b c d = fromIntegral a / fromIntegral b * (d - c) + c
    in transform x w rMin rMax :+ transform y h iMin iMax

mandelbrot :: Int -> Int -> Int -> Int -> RGB Float
mandelbrot w h x y =
    let c = pixelToComplex w h x y
        mandelbrot' iter z
          | iter >= 80 = HSL.hsl 0 0 0
          | realPart (abs z) >= 2 = HSL.hsl (iter / 80 * 360) 1 0.5
          | otherwise = mandelbrot' (iter + 1) (z * z + c)
     in mandelbrot' 0 (0 :+ 0)

rgbToWord :: RGB Float -> [Word8]
rgbToWord rgb =
    let rgbWord = fmap (truncate . (*255)) rgb
     in [channelRed rgbWord, channelGreen rgbWord, channelBlue rgbWord, 255]

genBitmap :: (Int -> Int -> Int -> Int -> RGB Float) -> Int -> Int -> ByteString
genBitmap coloring w h =
    pack $ mconcat (
        Data.List.unfoldr (
            \i -> if i >= w * h then Nothing else Just (rgbToWord $ coloring w h (i `mod` w) (i `div` w), i + 1)
        ) 0
    )

picture :: (Int -> Int -> Int -> Int -> RGB Float) -> Int -> Int -> Picture
picture coloring w h = bitmapOfByteString w h (BitmapFormat BottomToTop PxRGBA) (genBitmap coloring w h) False

main :: IO ()
main = do
    let width = 1200
        height = 800
        coloring = mandelbrot
     in display (InWindow "hfractal" (width, height) (0, 0)) white $ picture coloring width height