module Dungeons.Generator where
import Control.Monad (guard, replicateM)
import Data.List (sortBy)
import Data.Function (on)
import System.Random (randomRIO)

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
vnorm v@(Vector x y) = Vector (x / l) (y / l) where l = vmag v

vmag :: Vector -> Float
vmag (Vector x y) = sqrt (x * x + y * y)

topright :: Room -> Location
topright (Room (Location x y) width height) = Location (x + width) (y + height)

overlapping :: (Float, Float) -> (Float, Float) -> Bool
overlapping (a, b) (c, d) = (a <= c && c <= b) || (a <= d && d <= b) ||
                            (c <= a && a <= d) || (c <= b && b <= d)

atLeastNAway :: Float -> (Float, Float) -> (Float, Float) -> Bool
atLeastNAway n (a, b) (c, d) = overlapping (a - n, b + n) (c, d)

roomCloserThanN :: Float -> Room -> Room -> Bool
roomCloserThanN n r1 r2 = atLeastNAway n (minx1, maxx1) (minx2, maxx2) && 
                          atLeastNAway n (miny1, maxy1) (miny2, maxy2)
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

moveNAway :: Float -> Room -> [Room] -> Room
moveNAway n room otherRooms = if any (roomCloserThanN n room) otherRooms
                              then moveNAway n (moveRoom room direction) otherRooms
                              else room
    where otherCenter = combinedCenter otherRooms
          c = center room
          direction = vnorm $ vdiff c otherCenter

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

bufferAllRooms :: Float -> [Room] -> [Room]
bufferAllRooms n rooms = if null intersectingPairs
                         then rooms
                         else bufferAllRooms n (newRooms (fst (head intersectingPairs)))
    where intersectingPairs = filter (uncurry $ roomCloserThanN n) (pairs rooms)
          newRooms movingRoom = others ++ [moveNAway n movingRoom others]
              where others = filter (/= movingRoom) rooms

allConnections :: [a] -> (a -> a -> Float) -> [(a, a, Float)]
allConnections nodes distance = map addDistance (pairs nodes)
    where addDistance (r1, r2) = (r1, r2, distance r1 r2)

connected :: (Eq a) => [(a, a)] -> a -> a -> Bool
connected connections start end = withKnownConnections [start]
    where withKnownConnections connectedNodes = end `elem` connectedNodes || 
                                                (not (null newConnections) && 
                                                     withKnownConnections 
                                                     (connectedNodes ++ newConnections))
              where newConnectionsFrom i = (map fst . filter ((== i) . snd) $ connections) ++
                                           (map snd . filter ((== i) . fst) $ connections)
                    newConnections = filter (`notElem` connectedNodes) . 
                                     concatMap newConnectionsFrom $ connectedNodes

nextKruskal :: (Eq a) => [(a, a, Float)] -> [(a, a)] -> (a, a)
nextKruskal connections madeConnections = (r1, r2)
    where (r1, r2, distance) = head options
          options = sortBy (compare `on` third) . filter notBothConnected $ connections
          third (a, b, c) = c
          notBothConnected (r1, r2, _) = not $ connected madeConnections r1 r2

kruskal :: (Eq a, Show a) => [a] -> (a -> a -> Float) -> [(a, a)]
kruskal nodes distance = iterate grabNew [] !! (length nodes - 1)
    where grabNew madeConnections = nextKruskal possibleConnections madeConnections : madeConnections
          possibleConnections = allConnections nodes distance


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

randomRooms :: IO [Room]
randomRooms = replicateM 14 $ randomRoom (-10, 10) (-10, 10) (15, 20) (15, 20)
