module Main (main) where 

import Data.Monoid (mconcat, mappend)
import Graphics.Gloss (Picture(Translate, Line), rectangleWire, display, Display(InWindow), white)

import Dungeons.Generator (randomRooms, bufferAllRooms, corridors, center, 
                           Room(..), Location(..)) 

renderRoom :: Room -> Picture
renderRoom r@(Room _ width height) = 
    Translate x y $ rectangleWire width height
        where Location x y = center r

renderCorridor :: (Room, Room) -> Picture 
renderCorridor (room1, room2) = Line [(x, y), (x2, y2)] -- non-manhattan
    where Location x y = center room1
          Location x2 y2 = center room2

main :: IO ()
main = do
  rooms <- randomRooms
  let newRooms = bufferAllRooms 5 rooms
      roomsPic = mconcat (map renderRoom newRooms)
      corridorsPic = mconcat (map renderCorridor (corridors newRooms))
      picture = mappend roomsPic corridorsPic
  display (InWindow "Rooms" (400, 400) (10, 10)) white picture
