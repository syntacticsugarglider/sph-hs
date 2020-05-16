module Main where
import Display (display, idle)
import Lib
import Graphics.UI.GLUT
import Linear.V2
import Control.DeepSeq
import System.Exit
import System.Random
import Data.IORef

num_points :: Int
iter :: Int
chunks :: Int
num_points = 500
iter = 1000
chunks = 10

main :: IO ()
main = guiMain num_points iter

cliMain :: ([Particle] -> [Particle]) -> Int -> Int -> IO ()
cliMain f nps iters = do
  particles <- simInit nps
  let sol = iterate f particles !! iters
  sol `deepseq` putStrLn "Done."

guiMain :: Int -> Int -> IO ()
guiMain nps iters = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $=
    Size (fromIntegral window_width) (fromIntegral window_height)
  _window <- createWindow "SPH"
  reshapeCallback $= Just reshape
  init_

  particles <- simInit nps
  ps <- newIORef $ particles
  iterRef <- newIORef $ (0 :: Int)
 
  displayCallback $= display ps
  idleCallback $= Just (idle (num_points `div` chunks) iters ps iterRef)
  mainLoop

simInit :: Int -> IO [Particle]
simInit nps = do
  gen <- getStdGen

  let jitters = randoms gen :: [Double]
      points = take nps $ initPoints jitters

  return $ makeParticles points

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
 
init_ :: IO ()
init_ = do
  attachMenu RightButton (Menu [MenuEntry "Exit" (exitWith ExitSuccess)])

initPoints :: [Double] -> [(Double, Double)]
initPoints jitters = zipWith (\(x, y) j -> (x+j, y)) makePoints jitters
  where makePoints :: [(Double, Double)]
        makePoints = [(x, y) | y <- [view_width/4, view_width/4
                                     + h..view_height-eps*2],
                               x <- [eps, eps+h..view_width/2]]

makeParticles :: [(Double, Double)] -> [Particle]
makeParticles pts = map maker pts
  where maker :: (Double, Double) -> Particle
        maker (x, y) = Particle (V2 x y) (V2 0 0) (V2 0 0) 0 0