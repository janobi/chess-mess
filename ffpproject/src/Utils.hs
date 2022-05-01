module Utils where

import Control.Monad
import System.Random
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- | shortcut function to quickly log anything "showable" to the app's console window within a non-IO monad. A visual marker "### LOG: ###" with extra indentation is added for quick visual distinction from the regular server logs
debugLog :: (MonadIO m, Show a) => a -> m ()
debugLog msg = liftIO $ putStrLn $ "     ### LOG: ### " ++ show msg

-- | more declarative shortcut function to get the X component of a 'UI.Point'
x :: UI.Point -> Double
x = fst

-- | more declarative shortcut function to get the y component of a 'UI.Point'
y :: UI.Point -> Double
y = snd

-- | get the absolute position of a square cell in a grid based of its position in the grid, the cell size and the grid's offset
getSquarePosition :: UI.Point -> Double -> Double -> Double -> UI.Point
getSquarePosition (gridOffsetX, gridOffsetY) squareWidth xPos yPos =
    (squareWidth * xPos + gridOffsetX,
     squareWidth * yPos + gridOffsetY)

-- | get the center point of a square with the given position/offset and side-length
getSquareCenter :: UI.Point -> Double -> UI.Point
getSquareCenter (x,y) w = getRectCenter (x,y) (w,w)

-- | get the center point of a rectangle with the given position/offset and size
getRectCenter :: UI.Point -> UI.Point -> UI.Point
getRectCenter (x,y) (w,h) = (x + w/2, y + h/2)

-- | check if a given point is within a rectangle given by the top-left and bottom-right corner
isPointInRect :: UI.Point -> UI.Point -> UI.Point -> Bool
isPointInRect point rectTopLeft rectBottomRight =
    x point >= x rectTopLeft     && y point >= y rectTopLeft     &&
    x point <= x rectBottomRight && y point <= y rectBottomRight

-- | get the bottom-right point of a rectangle of given top-left position and size
getBottomRightPoint :: UI.Point -> UI.Point -> UI.Point
getBottomRightPoint topLeft size = (x topLeft + x size, y topLeft + y size)

-- | helper function to replace specific element in a list
replaceInList :: Int -> a -> [a] -> [a]
replaceInList pos obj list = let (ys,zs) = splitAt pos list in ys ++ [obj] ++ tail zs

-- | add two given Points / 2D Vectors and return the resulting Point / 2D Vector
addPoints :: UI.Point  -> UI.Point  -> UI.Point
addPoints (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- | apply a given list of type-preserving functions sequentially to the given object via foldl
applyFunctList :: [a -> a] -> a -> a
applyFunctList fnct obj = foldl (\ o f -> f o) obj fnct

-- | next random from gen directly
getNextRands  :: Int -> ([Float], [Float]) -> ([Float], [Float])
getNextRands 0 (y, x) = (y, x)
getNextRands amount (y, x) = getNextRands (amount - 1) (y ++ [head x], tail x)

-- | remove the nth element from a list: clean recursion approach from https://stackoverflow.com/questions/61492874/how-do-you-delete-the-nth-element-in-a-list-in-haskell
remove :: Int -> [a] -> [a]
remove _ []     = []
remove 0 (x:xs) = xs
remove n (x:xs) = x : remove (n-1) xs
