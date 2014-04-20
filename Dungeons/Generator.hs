{-# LANGUAGE BangPatterns #-}
module Dungeons.Generator where
import Control.Monad (guard, replicateM)
import Data.Monoid (mconcat)
import System.Random (randomRIO)
import Graphics.Gloss (Picture(Translate), rectangleWire, display, Display(InWindow), white)

data Location = Location { x :: !Float, y :: !Float } deriving (Show, Eq)

data Room = Room { 
      bottomleft :: !Location,
      width :: !Float,
      height :: !Float
    } deriving (Show, Eq)

data Vector = Vector { xdiff :: !Float, ydiff :: !Float } deriving (Show, Eq)

vdiff :: Location -> Location -> Vector
vdiff (Location x1 y1) (Location x2 y2) = Vector (x1 - x2) (y1 - y2)

vinverse :: Vector -> Vector
vinverse (Vector x y) = Vector (-x) (-y)

vnorm :: Vector -> Vector
vnorm (Vector x y) = Vector (x / l) (y / l)
    where l = sqrt (x * x + y * y)

topright :: Room -> Location
topright (Room (Location x y) width height) = Location (x + width) (y + height)

overlapping :: (Float, Float) -> (Float, Float) -> Bool
overlapping (a, b) (c, d) = (a <= c && c <= b) || (a <= d && d <= b) ||
                            (c <= a && a <= d) || (c <= b && b <= d)

roomOverlap :: Room -> Room -> Bool
roomOverlap r1 r2 = overlapping (minx1, maxx1) (minx2, maxx2) && 
                    overlapping (miny1, maxy1) (miny2, maxy2)
    where Location minx1 miny1 = bottomleft r1
          Location minx2 miny2 = bottomleft r2
          Location maxx1 maxy1 = topright r1
          Location maxx2 maxy2 = topright r2

center :: Room -> Location
center (Room (Location x y) width height) = Location (x + width / 2) (y + height / 2)

combinedCenter :: [Room] -> Location
combinedCenter = averageLocation . map center 
    where averageLocation ls = uncurry Location . factor (length ls) $ 
                               foldl combine (0, 0) ls
          combine (x, y) (Location x2 y2) = (x + x2, y + y2)
          factor n (x, y) = (x / fromIntegral n, y / fromIntegral n)

moveRoom :: Room -> Vector -> Room
moveRoom (Room (Location x y) width height) (Vector dx dy) =
    Room (Location (x + dx) (y + dy)) width height

moveAway :: Room -> [Room] -> Room
moveAway room otherRooms = if any (roomOverlap room) otherRooms
                           then moveAway (moveRoom room direction) otherRooms
                           else room
    where otherCenter = combinedCenter otherRooms
          c = center room
          direction = vnorm $ vdiff c otherCenter

-- todo add spacing
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

seperateAllRooms :: [Room] -> [Room]
seperateAllRooms rooms = if intersectingPairs == []
                         then rooms
                         else seperateAllRooms (newRooms (fst (head intersectingPairs)))
    where intersectingPairs = filter (uncurry roomOverlap) (pairs rooms)
          newRooms movingRoom = others ++ [moveAway movingRoom others]
              where others = filter (/= movingRoom) rooms

randomLocation :: (Float, Float) -> (Float, Float) -> IO Location
randomLocation xbounds ybounds = do
  x <- randomRIO xbounds
  y <- randomRIO ybounds
  return $ Location x y

randomRoom :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> IO Room
randomRoom (minX, maxX) (minY, maxY) widthbounds heightbounds = do
  width <- randomRIO widthbounds
  height <- randomRIO heightbounds
  location <- randomLocation (minX, maxX - width) (minY, maxY - height)
  return $ Room location width height

renderRoom :: Room -> Picture
renderRoom (Room (Location x y) width height) = 
    Translate x y $ rectangleWire width height

randomRooms :: IO [Room]
randomRooms = replicateM 14 $ randomRoom (-10, 10) (-10, 10) (15, 20) (15, 20)

main :: IO ()
main = do
  rooms <- randomRooms
  let newRooms = seperateAllRooms rooms
  display (InWindow "Rooms" (400, 400) (10, 10)) white
              . mconcat . map renderRoom $ newRooms
