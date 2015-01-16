{-# LANGUAGE BangPatterns #-}

-- cabal install juicypixels
-- ghc -O2 -o mandelbrot.exe mandelbrot.hs

import Codec.Picture.Png            -- juicypixels package
import Codec.Picture.Types          -- juicypixels package
import Data.Array
import Data.ByteString.Lazy
import Data.Complex
import System.Environment(getArgs)

-- square of the magnitude of a Complex; avoids a sqrt() operation
magnitude2 :: RealFloat a => Complex a -> a
magnitude2 z = (realPart z) * (realPart z) + (imagPart z) * (imagPart z)

-- Julia Set quadratic polynomial function
-- for Mandelbrot, call as: julia z z
-- for Julia Set, call as: julia z c
julia :: RealFloat a => Complex a -> Complex a -> Int
julia !z !c = f 0 z c where
    f !n !z !c
        | n >= 255           = n
        | magnitude2 z > 4.0 = n
        | otherwise          = f (n+1) (z * z + c) c
        
-- rgb8palette :: [PixelRGB8]
rgb8palette = listArray (0,255) [PixelRGB8 (47 * n) (23 * n) (17 *  n) |  n <- [0..255]]

juliaImage :: Int -> Int -> Complex Double -> Double -> Image PixelRGB8
juliaImage width height center scale =
    generateImage (f (fromIntegral width) (fromIntegral height) center scale) width height where
      f :: Double -> Double -> Complex Double -> Double -> Int -> Int -> PixelRGB8
      f w h zc zr i j = rgb8palette ! (julia z z)  where
        z = (zl + (fromIntegral i) * dz) :+ (zt - (fromIntegral j) * dz)
        c  = 0.285 :+ 0.01   -- Julia constant
        r = 0.5 * (max w h)
        dz = zr / r
        zl = (realPart zc) - w * 0.5 * dz
        zt = (imagPart zc) + h * 0.5 * dz

    
data Options = Options {
    help        :: Bool,
    width       :: Int,
    height      :: Int,
    center      :: Complex Double,
    scale       :: Double
} deriving Show

usage_options =
    "Writes a rendering of the Mandelbrot or Julia set as PNG to stdout.\n\
    \\n\
    \usage: mandelbrot [-w width] [-h height] [-c rcenter icenter] [-s scale]\n\
    \    --help         this help text\n\
    \    -w width       width of image, in pixels\n\
    \    -h height      height of image, in pixels\n\
    \    -c rcenter icenter  complex location of center\n\
    \    -s scale       1/2 width (or height) on complex plane\n"

parse_options :: [String] -> Options -> Options
parse_options args options  = f options args where
    f options ("--help":as) = f (options {help = True}) as
    f options ("-w":a:as)   = f (options {width = read a}) as
    f options ("-h":a:as)   = f (options {height = read a}) as
    f options ("-c":a:b:as) = f (options {center = ((read a) :+ (read b))}) as
    f options ("-s":a:as)   = f (options {scale = read a}) as
    f options _             = options

main = do
    args <- getArgs
    let options = parse_options args (Options False 1024 512 (0.0 :+ 0.0) 2.0)

    if (help options)
      then
        Prelude.putStr usage_options
      else
        Data.ByteString.Lazy.putStr $ encodePng $
          juliaImage (width options) (height options) (center options) (scale options)
