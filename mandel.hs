import Graphics.GD
import Control.Concurrent.ParallelIO

data Complex = C !Double !Double
--data Square = S !Double !Double !Double !Double
--data Area = A !Int !Int !Int !Int
--data Coord = Coord !Int !Int

isMandel :: Int -> Complex -> Bool
isMandel maxiter (C c1 c2) | c1 - sqrt p + 2*p < 0.25 || c1plus * c1plus + c2n < 0.0625 = True
                           | otherwise = isMandel_ maxiter (C 0.0 0.0)
  where c2n = c2 * c2
        c1plus = c1 + 1
        c1minus = c1 - 0.25
        p = c1minus * c1minus + c2n
        isMandel_ 0 _ = True
        isMandel_ maxiter (C z1 z2) | z1n > 4.0 = False
                                    | (z1n + z2n) > 4.0 = False
                                    | otherwise = isMandel_ (maxiter - 1) (C (z1n - z2n + c1) (2.0 * z1 * z2 + c2))
          where z2n = z2 * z2
                z1n = z1 * z1

mandelbrot :: Image -> Int -> Double -> Double -> IO ()
mandelbrot image maxiter h sx = mapM_ setPix (filter (isMandel maxiter) [(C x y)|x <- [(sx - 580.0)/250.0, (sx + 1 - 580.0)/250.0 ..(sx + h - 580.0)/250.0], y <- [-1.6, -1.596..1.6]])
  where setPix (C x y) = setPixel (truncate (x*250.0+580.0), truncate (y*250.0+400.0)) 0xffffff image

mandelbrotPar :: Double -> Image -> Int -> Double -> [IO ()]
mandelbrotPar h image maxiter tasknum = map (mandelbrot image maxiter step) [0.0, step..h]
  where step = h / tasknum

main = do
  image <- newImage (800, 800)
  parallel_ $ mandelbrotPar 800.0 image maxiter tasknum
  stopGlobalPool
  savePngFile "mandelbrot.png" image
    where maxiter = 5000
          tasknum = 40.0

