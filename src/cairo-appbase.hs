-- Playing with layout of campers who do not like each other, and want to be
-- as far from each other as possible.
-- by Alexander Iliev
--
-- Based on Cairo demo app by Conrad Parker
-- which is:
-- Based on Gtk2Hs/demo/cairo/Drawing2.hs
-- Author: Johan Bockgård <bojohan@dd.chalmers.se>
--
-- Licensed under BSD3
--

import qualified System.Glib.Types as GTypes
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Glade as Glade
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M

import qualified Graphics.UI.Gtk.Abstract.Widget as Widget

import Paths_cairo_appbase as My
import Control.Monad.Trans (liftIO)
import IO (stdout, hFlush)
import List ( (\\) )

import qualified Data.List.Extras.Argmax            as Argmax

cCANVAS_SIDE :: Int
cCANVAS_SIDE = 30


windowWidth, windowHeight :: Int
windowWidth   = 750
windowHeight  = 750


-------------------------------------
-- the campers layout algorithm

type Point = (Int, Int)

-- slight problem: generates the list in reverse.
campingLocations boundingRect start =
    iterate (\locs -> nextPoint boundingRect sum locs : locs) [start]

nextPoint :: (Int, Int) -> ([Double] -> Double) -> [Point] -> Point
nextPoint boundingRect distCombineF curPoints =
    let candidatePoints = (allPoints boundingRect \\ curPoints)
    in Argmax.argmax (\pt -> distCombineF $ map (dist pt) $ curPoints)
                        candidatePoints

-- square root of the sum of the squares.
sqrtSumSqrs :: [Double] -> Double
sqrtSumSqrs = sqrt . sum . map (^ 2)


-- | all the points within a bounding rect
allPoints (maxX, maxY) = [(x,y) | x <- [0..maxX], y <- [0..maxY]]

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt ( (real(x2-x1)^2) + (real(y2-y1)^2) )


-----------------------------------

-- Display image in window
main = do
  G.initGUI
  (window, canvas) <- setupGUIBoilerplate

  -- seems to be enabled by default
  -- G.widgetAddEvents canvas [Widget.ButtonPressMask]
  canvas `G.on` G.buttonPressEvent $ G.tryEvent $
    do (x,y) <- G.eventCoordinates  -- this works on the Reader monad in EventM
       (tranX, tranY) <- liftIO $ translateCoords canvas x y
       button <- G.eventButton
       liftIO $ -- need liftIO here as the top level Monad is EventM, not IO
          putStrLn $ show button ++ " pressed at win " ++ show (x,y) ++
          ", canvas " ++ show (tranX,tranY)
       liftIO $ hFlush stdout

  G.widgetShowAll window
  let points = (!! 50) $ campingLocations (cCANVAS_SIDE,cCANVAS_SIDE) (12,7)
  G.onExpose canvas $ const (updateCanvas canvas points)
  G.mainGUI



-- | Translate canvas coords from window domain to user domain.
translateCoords :: G.DrawingArea -> Double -> Double -> IO (Int,Int)
translateCoords canvas winX winY =
    do (winWidth, winHeight) <- G.widgetGetSize canvas
       let (x,y) = M.transformPoint
                (M.invert $ transformMatrix winWidth winHeight) (winX, winY)
       return $ (round x, round y)



updateCanvas :: G.DrawingArea -> [Point] -> IO Bool
updateCanvas canvas points = do
  win <- G.widgetGetDrawWindow canvas
  (width, height) <- G.widgetGetSize canvas
  G.renderWithDrawable win $
      example width height points
  return True

----------------------------------------------------------------

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

keepState render = do
  C.save
  render
  C.restore

drawText :: Point -> String -> C.Render()
drawText (x,y) text = do
    transform <- C.getMatrix
    C.setMatrix $ M.scale 1 (-1) transform + M.Matrix 0 0 0 0 0 1600
    C.moveTo (real x) (real y)
    C.showText text
    C.setMatrix transform

drawCircle :: Double -> Double -> Point -> C.Render()
drawCircle r darkness (x,y) = do
  C.setSourceRGBA (darkness) 0 ((5/7)*darkness) 0.5
  C.arc (real x) (real y) r 0 (2 * pi)
  fillStroke

drawRectangle x y w h = do
  C.rectangle x y w h
  fillStroke

stroke =
  keepState $ do
  C.setSourceRGBA 0 0 0 0.7
  C.stroke

fillStroke = do
  C.fillPreserve
  stroke

----------------------------------------------------------------

-- Example

example width height points = do
  prologue width height
  example_sasho points

example_sasho points = do
  C.setFontSize 0.5
  let   numPoints = length points
        indexedPts = zip [1..] points
  foreach indexedPts
    --(\( i, (x,y) ) -> drawText (x,y) (show i))
     (\( i, pt ) -> drawCircle 0.25 (real i/real numPoints) pt)

-- | Matrix to apply to user space coords to get window coords
transformMatrix wWidth wHeight =
    let width   = real cCANVAS_SIDE
        height  = real cCANVAS_SIDE
        scaleX  = realToFrac wWidth  / width
        scaleY  = realToFrac wHeight / height
    in M.Matrix
            scaleX 0 0 (-scaleY)    -- scale and flip Y-axis to increase upwards
--            (fromIntegral wWidth / 2) ((fromIntegral wHeight / 2))  -- shift to put origin in middle
            0 (fromIntegral wHeight)    -- shift horiz axis to bottom of window.


-- Set up stuff
prologue :: Int -> Int -> C.Render ()
prologue wWidth wHeight = do
  let width   = real cCANVAS_SIDE
      height  = real cCANVAS_SIDE
      xmax    = width
      xmin    = - xmax
      ymax    = height
      ymin    = - ymax
      scaleX  = realToFrac wWidth  / width
      scaleY  = realToFrac wHeight / height
      -- Matrix to apply to user space coords to get window coords
      matrix = transformMatrix wWidth wHeight

  -- style and color
  C.setLineCap C.LineCapRound
  C.setLineJoin C.LineJoinRound
  C.setLineWidth $ 1 / max scaleX scaleY
  C.setSourceRGBA 0.7 0.2 0.5 0.5

  -- Set up user coordinates
  -- C.scale scaleX scaleY
  -- center origin
  -- C.translate (width / 2) (height / 2)
  C.transform matrix

  grid xmin xmax ymin ymax


-- Grid and axes
grid xmin xmax ymin ymax =
  keepState $ do
  C.setSourceRGBA 0 0 0 0.7
  -- axes
  C.moveTo 0 ymin; C.lineTo 0 ymax; C.stroke
  C.moveTo xmin 0; C.lineTo xmax 0; C.stroke
  -- grid
  C.setDash [0.01, 0.99] 0
  foreach [xmin .. xmax] $ \ x ->
      do C.moveTo x ymin
         C.lineTo x ymax
         C.stroke

real = fromIntegral

----------------------------------------------------------------


setupGUIBoilerplate = do
  -- load up the glade file
  filename <- My.getDataFileName "data/main.glade"
  windowXmlM <- Glade.xmlNew filename
  let windowXml = case windowXmlM of
        (Just wX) -> wX
        Nothing -> error ("can't find the glade file " ++ filename)
      get :: (G.WidgetClass widget) => (GTypes.GObject -> widget) -> String -> IO widget
      get = Glade.xmlGetWidget windowXml

  -- get a handle on widgets from the glade file
  window <- get G.castToWindow "window1"

  -- set up File-> New
  new1 <- get G.castToMenuItem "new1"
  G.onActivateLeaf new1 $ myNew

  -- set up the File-> Open dialog
  open1 <- get G.castToMenuItem "open1"
  openDialog <- get G.castToFileChooserDialog "opendialog"
  G.onActivateLeaf open1 $ G.widgetShow openDialog
  G.onResponse openDialog $ myFileOpen openDialog

  -- set up the File-> Save_As dialog
  save1 <- get G.castToMenuItem "save1"
  save_as1 <- get G.castToMenuItem "save_as1"
  saveDialog <- get G.castToFileChooserDialog "savedialog"
  G.onActivateLeaf save_as1 $ G.widgetShow saveDialog
  G.onActivateLeaf save1 $ G.widgetShow saveDialog
  G.onResponse saveDialog $ myFileSave saveDialog

  -- set up Edit menu
  cut1 <- get G.castToMenuItem "cut1"
  G.onActivateLeaf cut1 $ myCut
  copy1 <- get G.castToMenuItem "copy1"
  G.onActivateLeaf copy1 $ myCopy
  paste1 <- get G.castToMenuItem "paste1"
  G.onActivateLeaf paste1 $ myPaste
  delete1 <- get G.castToMenuItem "delete1"
  G.onActivateLeaf delete1 $ myDelete

  -- set up the Help-> About dialog
  about1 <- get G.castToMenuItem "about1"
  aboutdialog1 <- get G.castToAboutDialog "aboutdialog1"
  G.onActivateLeaf about1 $ G.widgetShow aboutdialog1
  G.onResponse aboutdialog1 $ const $ G.widgetHide aboutdialog1

  -- fix size
  --   G.windowSetResizable window False
  G.widgetSetSizeRequest window windowWidth windowHeight

  -- quit on File-> Quit menu selection
  quit1 <- get G.castToMenuItem "quit1"
  G.onActivateLeaf quit1 $ G.widgetDestroy window
  G.onDestroy window G.mainQuit

  -- set up the canvas
  canvas <- get G.castToDrawingArea "drawingarea1"

  return (window, canvas)



-- Write image to file
writePng :: FilePath -> IO ()
writePng outFile =
  C.withImageSurface C.FormatARGB32 width height $ \ result -> do
      let points = (!! 50) $ campingLocations (cCANVAS_SIDE,cCANVAS_SIDE) (12,7)
      C.renderWith result $ example width height points
      C.surfaceWriteToPNG result outFile
  where width  = windowWidth
        height = windowHeight

myNew :: IO ()
myNew = putStrLn "New"

myFileOpen :: G.FileChooserDialog -> G.ResponseId -> IO ()
myFileOpen fcdialog response = do
  case response of
    G.ResponseOk -> do Just filename <- G.fileChooserGetFilename fcdialog
                       putStrLn $ "Opening " ++ filename
    G.ResponseCancel -> putStrLn "Cancelled!"
    G.ResponseDeleteEvent -> putStrLn "FileChooserDialog Deleted!"
    G.ResponseClose -> putStrLn "Closed!"
  G.widgetHide fcdialog

fileAction :: (FilePath -> IO()) -> G.FileChooserDialog -> G.ResponseId -> IO ()
fileAction fileProc fcdialog response = do
  case response of
    G.ResponseOk -> do  Just filename <- G.fileChooserGetFilename fcdialog
                        fileProc filename
    G.ResponseCancel -> putStrLn "Cancelled!"
    G.ResponseDeleteEvent -> putStrLn "FileChooserDialog Deleted!"
    G.ResponseClose -> putStrLn "Closed!"
  G.widgetHide fcdialog

myFileSave :: G.FileChooserDialog -> G.ResponseId -> IO ()
myFileSave = fileAction $ \filename -> do
    putStrLn $ "Saving to file " ++ filename
    writePng filename


myCut :: IO ()
myCut = putStrLn "Cut"

myCopy :: IO ()
myCopy = putStrLn "Copy"

myPaste :: IO ()
myPaste = putStrLn "Paste"

myDelete :: IO ()
myDelete = putStrLn "Delete"
