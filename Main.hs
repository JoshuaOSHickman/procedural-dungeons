module Main (main) where 

import Data.Monoid (mconcat)
import Graphics.Gloss (Picture(Translate), rectangleWire, display, Display(InWindow), white)

import Dungeons.Generator (randomRooms, bufferAllRooms, Room(..), Location(..)) 

renderRoom :: Room -> Picture
renderRoom (Room (Location x y) width height) = 
    Translate x y $ rectangleWire width height

main :: IO ()
main = do
  rooms <- randomRooms
  let newRooms = bufferAllRooms 5 rooms
  display (InWindow "Rooms" (400, 400) (10, 10)) white
              . mconcat . map renderRoom $ newRooms
