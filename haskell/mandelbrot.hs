{-# LANGUAGE BangPatterns #-}

-- cabal install juicypixels
--

import Codec.Picture.Png            -- juicypixels package
import Codec.Picture.Types          -- juicypixels package
import Data.Array
import Data.Complex
import Data.Word
import System.IO
import System.Environment


-- mb :: RealFloat a => Complex a -> a
mb :: Complex Double -> Int
mb !z = mbs 0 z z where
    mbs !n !z !c
        | n >= 255         	= n
        | magnitude z > 2.0 = n
        | otherwise         = mbs (n+1) (z * z + c) c
        

-- rgb8palette :: [PixelRGB8]
rgb8palette2 = listArray (0,255) [PixelRGB8 (47 * n) (23 * n) (17 *  n) |  n <- [0..255]]
rgb8palette = listArray (0,255) [PixelRGB8
    (f n 1  3)
    (f n 1  11)
    (f n 1  7)
    |  n <- [0..255]]  where
    f :: Double -> Double -> Double -> Pixel8
    f x nu phi = floor $ 255 * (sin (2 * pi * (nu * x / 64 + phi/12)) + 1) / 2
    
    
main = do

    let img = generateImage f w h  where
        g :: Int -> Int -> PixelRGB8
        g x y = rgb8palette ! (mod (div x 16) 256)
        
        f :: Int -> Int -> PixelRGB8
        f x y = rgb8palette ! (mb z)  where
            z = (pc x wr x0) :+ (pc y hr y0) 
            pc x r x0 = (fromIntegral (x - r)) * s + x0 

        x0  =  -1.0   :: Double
        y0  =   0.0   :: Double
        s   =   0.003 :: Double   -- real or imaginary units per pixel
        
        w   = 1300 :: Int
        h   =  700 :: Int
        wr  = div w 2
        hr  = div h 2

    writePng "out.png" img




{-
-- [x0, y0, s] = map read getArgs :: [Double]

main = do
    png <- Data.ByteString.readFile "cairo-clock-icon.png"
    let img = decodePng png
    case img of
        Left x -> System.IO.putStrLn x
        Right x -> do
            case x of
                ImageRGBA8 x  -> do
                    mi <- thawImage x
                    mi <- createMutableImage 48 48 (PixelRGBA8 0 255 0 255)
                    writePixel mi 4 4 (PixelRGBA8 255 0 0 255) 
                    -- writePixel mi 4 5 (PixelRGBA8 1 1 1 255) 
                    i <- freezeImage mi
                    writePng "out.png" i
                    System.IO.putStrLn ((show $ imageHeight x) ++ " x " ++ (show $ imageWidth x))
-}
