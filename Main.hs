module Main (main) where 

import Data.Monoid (mconcat, mappend)
import Graphics.Gloss (rectangleWire, display, white,
                       Picture(Translate, Line), Display(InWindow))
import System.Environment (getArgs)

import Dungeons.Generator (randomRooms, bufferAllRooms, corridors, center, 
                           Room(Room), Location(Location))

renderRoom :: Room -> Picture
renderRoom r@(Room _ width height) = 
    Translate x y $ rectangleWire width height
        where Location x y = center r

renderCorridor :: (Room, Room) -> Picture 
renderCorridor (room1, room2) = Line [(x, y), (x2, y2)] -- non-manhattan
    where Location x y = center room1
          Location x2 y2 = center room2

renderCorridorManhattan :: (Room, Room) -> Picture
renderCorridorManhattan (room1, room2) = mappend horizontalLine verticalLine
    where Location x1 y1 = center room1
          Location x2 y2 = center room2
          horizontalLine = Line [(x1, y1), (x2, y1)] -- see, no change in y
          verticalLine = Line [(x2, y1), (x2, y2)] -- I'm thinking I could make this one array?

main :: IO ()
main = do
  args <- getArgs
  rooms <- randomRooms
  let newRooms = bufferAllRooms 5 rooms
      roomsPic = mconcat (map renderRoom newRooms)
      useManhattan = null args || head args == "manhattan"
      corridorRendering = if useManhattan then renderCorridorManhattan else renderCorridor
      corridorsPic = mconcat (map corridorRendering (corridors newRooms))
      picture = mappend roomsPic corridorsPic
  display (InWindow "Rooms" (400, 400) (10, 10)) white picture
