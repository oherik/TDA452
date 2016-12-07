import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Maybe

import Pages

import Expr



canWidth  = 300
canHeight = 300


---- I ----

-- parseElemToExpr :: Elem -> Maybe Expr
-- parseElemToExpr elem = do
--                       expr <- getValue elem
--                       if isJust expr
--                       then
--                       else error "Invalid expression"

-- reads expression from the given input element
-- draws the graph on the given canvas
readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw e canvas = do
                            val <- getValue e
                            let expr = readExpr (fromJust val)
                            if isJust expr then render canvas $ stroke $ path $ points (fromJust expr) 100.0 canvasSize
                            else error "Invalid expression"
                            where
                              canvasSize = (canWidth,canHeight)

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13

      ---- Part 2 ----
      -- type Point = (Double, Double)

      ---- H ----

points :: Expr -> Double -> (Int,Int) -> [Point]
points ex scale (width,height) =
  [(x, realToPix (eval ex (pixToReal x)))| x <- [0..(fromIntegral width-1)]]
  where
    -- converts a pixel x-coordinate to a real x-coordinate
    pixToReal :: Double -> Double
    pixToReal x = ((x - (fromIntegral width / 2)) * scale)
    -- converts a real y-coordinate to a pixel y-coordinate
    realToPix :: Double -> Double
    realToPix y = negate ((y) / scale + fromIntegral height / 2)  + fromIntegral height

      ---- I ----
      -- readAndDraw :: Elem -> Canvas -> IO ()
