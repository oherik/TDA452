import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

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
readAndDraw elem canvas = do
                            expr <- readExpr $ fromJust $ getValue elem
                            if isJust expr then path $ points $ expr 1 canWidth canHeight
                            else error "Invalid expression"
                            --få ett expr
                            -- om expr visa i min IO
                            --annars error


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
<<<<<<< HEAD
      -- points :: Expr -> Double -> (Int,Int) -> [Point]
=======
points :: Expr -> Double -> (Int,Int) -> [Point]
points ex scale (width,height) =
  [(x, realToPix (eval ex (pixToReal x)))| x <- [0..wPix-1]]
  where
    hPix = fromIntegral height / scale
    wPix = fromIntegral width / scale
    -- converts a pixel x-coordinate to a real x-coordinate
    pixToReal :: Double -> Double
    pixToReal x = (x * scale) - (fromIntegral width / 2)
    -- converts a real y-coordinate to a pixel y-coordinate
    realToPix :: Double -> Double
    realToPix y = negate ((y + fromIntegral height / 2) / scale) + hPix

      ---- I ----
      -- readAndDraw :: Elem -> Canvas -> IO ()
>>>>>>> 02567df30d836d02fa527ce5debd378dedc5de1a
