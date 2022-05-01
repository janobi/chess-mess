module CanvasAdds where

-- adds some canvas functions that Threepenny doesn't have. Based on by Threepenny's implementation of similar functions.

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Canvas

-- Canvas textBaseline property (code based on Threepenny's implementation of textAlign and the existing native (JS) textBaseline canvas property)
data TextBaseline = Alphabetic | Top | Hanging | Middle | Ideographic | Bottom deriving (Show, Eq, Read)

bToS :: TextBaseline -> String
bToS bsln = case bsln of
    Alphabetic -> "alphabetic"
    Top -> "top"
    Hanging -> "hanging"
    Middle -> "middle"
    Ideographic -> "ideographic"
    Bottom -> "bottom"

sToB :: String -> TextBaseline
sToB bsln = case bsln of
    "Alphabetic" -> Alphabetic
    "Top" -> Top
    "Hanging" -> Hanging
    "Middle" -> Middle
    "Ideographic" -> Ideographic
    "Bottom" -> Bottom
    _ -> Alphabetic

-- | text baseline / vertical alignment. Default is 'Alphabetic'.
textBaseline :: Attr Canvas TextBaseline
textBaseline = bimapAttr bToS sToB $ textBaselineStr
    where
        textBaselineStr :: Attr Canvas String
        textBaselineStr = fromObjectProperty "getContext('2d').textBaseline"

-- Canvas arcTo method (code based on Threepenny's implementation of lineTo and the existing native (JS) arcTo canvas method)
-- | Create an arc between two tangents on the canvas. From the previous point in an existing path, a line will be drawn around a circle with the given @radius@ so that @(x1,y1)@ is the unreached point and the line ends in @(x2,y2)@.
arcTo :: Point -> Point -> Double -> Canvas -> UI ()
arcTo (x1,y1) (x2,y2) r canvas = runFunction $ ffi "%1.getContext('2d').arcTo(%2, %3, %4, %5, %6)" canvas x1 y1 x2 y2 r

-- Canvas drawImage method with destination width and height parameters (code based on Threepenny's implementation of drawImage with only destination position and the existing native (JS) drawImage method with 5 arguments)
-- | Draw an image at the given position with the given size
drawImageScaled :: Element -> Point -> Point -> Canvas -> UI ()
drawImageScaled image (x,y) (w,h) canvas = runFunction $ ffi "%1.getContext('2d').drawImage(%2, %3, %4, %5, %6)" canvas image x y w h

-- Custom drawRoundedRect method for Canvas (based on the functionality of the native arcTo method)
-- | draw a path for a rectangle with corners rounded around a circle with the given @radius@, starting at the top-left corner @(x0,y0)@ with dimensions @(w,h)@. The path can be filled or stroked afterwards.
drawRoundedRect :: Point -> Point -> Double -> Canvas -> UI ()
drawRoundedRect (x0,y0) (w,h) radius canvas = do
    let x1 = x0 + w
    let y1 = y0 + h
    canvas # beginPath
    canvas # moveTo (x0+radius, y0)                  -- start after top-left corner
    canvas # lineTo (x1-radius, y0)                  -- draw top edge
    canvas # arcTo (x1, y0) (x1, y0+radius) radius   -- draw top-right corner
    canvas # lineTo (x1, y1-radius)                  -- draw right edge
    canvas # arcTo (x1, y1) (x1-radius, y1) radius   -- draw bottom-right corner
    canvas # lineTo (x0+radius, y1)                  -- draw bottom edge
    canvas # arcTo (x0, y1) (x0, y1-radius) radius   -- draw bottom-left corner
    canvas # lineTo (x0, y0+radius)                  -- draw left edge
    canvas # arcTo (x0, y0) (x0+radius, y0) radius   -- draw top-left corner
    canvas # closePath
    return ()

-- Shortcut for filling after drawRoundedRect
-- | fill a rectangle with corners rounded around a circle with the given @radius@, starting at the top-left corner @(x0,y0)@ with dimensions @(w,h)@.
fillRoundedRect :: Point -> Point -> Double -> Canvas -> UI ()
fillRoundedRect pos size radius canvas = do
    canvas # drawRoundedRect pos size radius
    canvas # fill

-- Shortcut for stroking after drawRoundedRect
-- | draw the outline of a rectangle with corners rounded around a circle with the given @radius@, starting at the top-left corner @(x0,y0)@ with dimensions @(w,h)@.
strokeRoundedRect :: Point -> Point -> Double -> Canvas -> UI ()
strokeRoundedRect pos size radius canvas = do
    canvas # drawRoundedRect pos size radius
    canvas # stroke
