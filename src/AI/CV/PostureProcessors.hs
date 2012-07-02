module AI.CV.PostureProcessors where

-- External imports
import AI.CV.ImageProcessors
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.CxCore
import Control.Arrow
import Control.Concurrent
import Control.Monad
import Control.Processor (processor, IOProcessor, IOSource)
import Data.Time.Clock
import Foreign.C.Types

-- Internal imports
import Paths

createInitialState :: Int -> Int -> (Int, Int, Int, Int) -> IO InternalState
createInitialState cor del (x,y,w,h) = do
  time <- getCurrentTime
  return ((CvRect (fI x) (fI y) (fI w) (fI h), cor, del, 0, 0), time)
 where fI = fromIntegral

-- Returns true if exactly one rectangle has been detected
-- or 5 seconds have passed since 't'.
detectedOrFiveSeconds :: UTCTime -> [CvRect] -> IO Bool
detectedOrFiveSeconds t []  = do
  time <- getCurrentTime
  let diff = diffUTCTime time t
  return (diff > 5)
detectedOrFiveSeconds _ [_] = return True
detectedOrFiveSeconds _ _   = return False

initialiseDetector :: Int -> InternalState -> IO (IOProcessor () InternalState)
initialiseDetector cam state = do
  fn <- getDataFileName "haarcascade_frontalface_alt.xml"
  return (detector cam fn state)

-- Detects the current person, calculates its position and size
-- and notifies the system when necessary.
detector :: Int -> String -> InternalState -> IOProcessor () InternalState
detector n fn state = camera n >>> haar fn >>> detectPosture state

runFinder :: Int -> IO (Maybe (Int, Int, Int, Int))
runFinder cam = do
   -- Run for a few seconds or until once face is detected
   finderProc <- initialiseFinder cam
   time       <- getCurrentTime
   res        <- runTill finderProc (detectedOrFiveSeconds time)
   case res of
     [CvRect x y w h] -> return (Just (fI x, fI y, fI w, fI h))
     _                -> return (Nothing :: Maybe (Int, Int, Int, Int))
 where fI = fromIntegral :: CInt -> Int
  
initialiseFinder :: Int -> IO (IOProcessor () [CvRect])
initialiseFinder cam = do
  fn  <- getDataFileName "haarcascade_frontalface_alt.xml"
  return (finder cam fn)

-- Runs the face detector on the first camera
finder :: Int -> String -> IOProcessor () [CvRect]
finder n fn = camera n >>> haar fn

-- Internal state is
-- CvRect -> Head position when sitting correctly
-- Int    -> Correction factor
-- Int    -> Delay
-- Int    -> wrong frames in the past 'Delay'
-- Int    -> missing frames in the past 'Delay'
type InternalState = (PostureState, UTCTime)
type PostureState = (CvRect, Int, Int, Int, Int)

-- Detects faces, compares with known references and notifies the system if the
-- user should correct the posture
detectPosture :: InternalState -> IOSource [CvRect] InternalState
detectPosture initialState = processor
  (\rects (state, time) -> do
     -- Delays for several milisecons if the previous call happened
     -- not too long ago
     time' <- getCurrentTime
     let diff = diffUTCTime time' time
         wait = 1000000 * (0.8 - diff)
     when (wait > 0) $ threadDelay $ round wait

     -- Processes the current internal state and calculates the new one
     let state' = process rects state

     -- Get a new timestamp and return the new state
     t' <- getCurrentTime
     return (state', t')
  )
  (\_           -> return initialState)
  (\s           -> return s)
  (\_           -> return ())

-- Calculates the new internal state from the previous one and the list of
-- detected faces
process :: [CvRect] -> PostureState -> PostureState
process es (rect, corr, delay, wrong, missing) =
   (rect, corr, delay, withinRange (0, delay) wrong', withinRange (0, delay) missing')
 where (wrong', missing') 
           | null es                             = (wrong - 1, missing + 1)
           | all (>= defaultMargin) corDistances = (wrong + 1, missing - 1)
           | otherwise                           = (0, 0)

       corDistances = map (\x -> x / (fromIntegral corr + 1)) distances

       calculateDistance :: CvRect -> Double
       calculateDistance e = let (d1, d2) = calculateDistances e rect
                             in d1 * d2

       distances    :: [ Double ]
       distances    = map calculateDistance es

-- Face detector image processor
haar :: String -> IOProcessor Image [CvRect]
haar fn = haarDetect fn 1.1 3 (HaarDetectFlag 1) (CvSize 40 40)
        
-- This value is used to compare the calculated values from the 'correct' ones.
-- It is corrected (or relaxed) using a value in the internal state.
defaultMargin :: Double
defaultMargin = 500.0

-- Auxiliary operations

-- Calculates the distance between two rectangles in terms of position and
-- size.
calculateDistances :: CvRect -> CvRect -> (Double, Double)
calculateDistances r1 r2 = (d1, d2)
 where d1 = sqrt (f1 + f2)
       d2 = sqrt (f3 + f4)
       f1 = varDistance (rectX r1)      (rectX r2)      ^ (2 :: Int)
       f2 = varDistance (rectY r1)      (rectY r2)      ^ (2 :: Int)
       f3 = varDistance (rectWidth r1)  (rectWidth r2)  ^ (2 :: Int)
       f4 = varDistance (rectHeight r1) (rectHeight r2) ^ (2 :: Int)

-- Calculates the distance between two points
varDistance :: CInt -> CInt -> Double
varDistance v1 v2 = abs (fromIntegral v1 - fromIntegral v2)

-- withinRange (b1, b2) a returns the closes value to a in the range [b1, b2]
withinRange :: Ord a => (a, a) -> a -> a
withinRange (a, b) x
 | x >= a && x <= b = x
 | x < a            = a
 | otherwise        = b

data DetectionStatus = DetectionOk
                     | DetectionWrong
                     | DetectionUnknown

detectionStatus :: InternalState -> DetectionStatus
detectionStatus ((_rct, _cr, d, w, m), _)
  | w + m >= d && w > d - 2  = DetectionWrong
  | m >= d                   = DetectionUnknown
  | otherwise                = DetectionOk
