import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Maybe

import Pages

import Expr
import Parsing



canWidth  = 300
canHeight = 300


main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    diff    <- mkButton "Differentiate"      -- The differentiate button
    zoomI    <- mkButton "Zoom in"      -- The differentiate button
    zoomO    <- mkButton "Zoom out"      -- The differentiate button
    scale    <- mkInput 20 "0.1"      -- The default scale

      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    buttons <- mkDiv
    row formula [fx,input]
    row buttons [draw,diff, zoomI, zoomO]
    column documentBody [canvas,formula,buttons]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- fromElem canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
    onEvent diff  Click $ \_    -> readAndDiff  input can
    onEvent zoomO Click $ \_    -> zoomIO input scale (0.01) can
    onEvent zoomI Click $ \_    -> zoomIO input scale (-0.01) can

      -- "Enter" key has code 13

      ---- Part 2 ----
      -- type Point = (Double, Double)

      ---- H ----
points :: Expr -> Double -> (Int,Int) -> [Point]
points ex scale (width,height) = map (\x -> (x,realToPix (eval ex (pixToReal x)))) [0..(fromIntegral width)]
  where
    -- converts a pixel x-coordinate to a real x-coordinate
    pixToReal :: Double -> Double
    pixToReal x = ((x - (fromIntegral width / 2)) * scale)
    -- converts a real y-coordinate to a pixel y-coordinate
    realToPix :: Double -> Double
    realToPix y = negate ((y) / scale + fromIntegral height / 2)  + fromIntegral height

---- I ----
-- reads expression from the given input element
-- draws the graph on the given canvas
readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw e canvas = do   val <- getProp e "value"
                            let expr = readExpr val
                            if isJust expr then
                              render canvas $ stroke $ path $ points (fromJust expr) 0.1 canvasSize
                            else
                              do
                                _ <- setProp e "value" "Invalid input"
                                render canvas $ stroke $ path []
                            where
                              canvasSize = (canWidth,canHeight)

---- J ----
zoomIO :: Elem -> Elem -> Double -> Canvas -> IO()
zoomIO e scale zoomstep canvas = do  currScale <- getProp scale "value"
                                     val <- getProp e "value"
                                     let expr = readExpr val
                                     let zoomScale = zoomstep + (read currScale)
                                     set scale
                                          [ prop "value" =: show zoomScale]
                                     drawWithScale (fromJust expr) canvas zoomScale


drawWithScale :: Expr -> Canvas -> Double -> IO ()
drawWithScale expr canvas zoomstep =
                           render canvas $ stroke $ path $ points expr zoomstep canvasSize
                           where
                             canvasSize = (canWidth,canHeight)
---- K ----
readAndDiff :: Elem -> Canvas -> IO ()
readAndDiff e canvas = do   val <- getProp e "value"
                            _ <- setProp e "value" $ showExpr $ simplify $ differentiate $ fromJust $ readExpr val
                            readAndDraw e canvas
