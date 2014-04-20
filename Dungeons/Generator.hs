module Dungeons.Generator where
import Control.Monad (guard, replicateM)
import Data.Monoid (mconcat)
import System.Random (randomRIO)
import Graphics.Gloss (Picture(Translate), rectangleWire, display, Display(InWindow), white)

data Location = Location { x :: Int, y :: Int } deriving (Show, Eq)

data Room = Room { 
      bottomleft :: Location,
      width :: Int,
      height :: Int
    } deriving (Show, Eq)

data Vector = Vector { xdiff :: Int, ydiff :: Int } deriving (Show, Eq)

vdiff :: Location -> Location -> Vector
vdiff (Location x1 y1) (Location x2 y2) = Vector (x1 - x2) (y1 - y2)

vinverse :: Vector -> Vector
vinverse (Vector x y) = Vector (-x) (-y)

topright :: Room -> Location
topright (Room (Location x y) width height) = Location (x + width) (y + height)

overlapping :: (Int, Int) -> (Int, Int) -> Bool
overlapping (a, b) (c, d) = (a <= c && c <= b) || (a <= d && d <= b)

roomOverlap :: Room -> Room -> Bool
roomOverlap r1 r2 = overlapping (minx1, maxx1) (minx2, maxx2) && 
                    overlapping (miny1, maxy1) (miny2, maxy2)
    where Location minx1 miny1 = bottomleft r1
          Location minx2 miny2 = bottomleft r2
          Location maxx1 maxy1 = topright r1
          Location maxx2 maxy2 = topright r2

center :: Room -> Location
center (Room (Location x y) width height) = Location (x + width `div` 2) (y + height `div` 2)

combinedCenter :: [Room] -> Location
combinedCenter = averageLocation . map center 
    where averageLocation ls = uncurry Location . factor (length ls) $ 
                               foldl combine (0, 0) ls
          combine (x, y) (Location x2 y2) = (x + x2, y + y2)
          factor n (x, y) = (x `div` n, y `div` n)

randomLocation :: (Int, Int) -> (Int, Int) -> IO Location
randomLocation xbounds ybounds = do
  x <- randomRIO xbounds
  y <- randomRIO ybounds
  return $ Location x y

randomRoom :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> IO Room
randomRoom (minX, maxX) (minY, maxY) widthbounds heightbounds = do
  width <- randomRIO widthbounds
  height <- randomRIO heightbounds
  location <- randomLocation (minX, maxX - width) (minY, maxY - height)
  return $ Room location width height

renderRoom :: Room -> Picture
renderRoom (Room (Location x y) width height) = 
    Translate (fromIntegral x) (fromIntegral y) $ 
    rectangleWire (fromIntegral width) (fromIntegral height)

randomRooms :: IO [Room]
randomRooms = replicateM 13 $ randomRoom (-50, 50) (-50, 50) (10, 20) (10, 20)

main :: IO ()
main = do
  randomRooms >>= 
       display (InWindow "Rooms" (400, 400) (10, 10)) white 
                   . mconcat . map renderRoom
