module Gcd ( testGcdLog
           , Event (..)
           , Log
           , LogEntry (..)
           , State
           , GcdState (..)
           , genGcdLogInFile  
           ) 
       where 
         
import System.Random
import Data.Maybe

max_par_val = 100

type EName = String
type EParam = Int
type EParams = (PArity, Maybe [EParam])
type PArity = Int

data Event = Event EName EParams
             deriving (Show, Eq, Read)
type EventsSpec = [Event]

data GcdRealState = RState { getX'    :: Maybe Int
                           , getY'    :: Maybe Int
                           , getR'    :: Maybe Int
                           , getComb' :: Maybe Int
                           }

type GcdExtState = Maybe GcdState

data GcdState = State { getX :: Maybe Int
                      , getY :: Maybe Int
                      , getR :: Maybe Int
                      }
                deriving (Show, Eq, Read)
data LogEntry = LogE { evtFrL :: Event 
                     , stFrL  :: GcdState
                     }
              deriving (Show, Eq, Read)
type Log = [LogEntry]

type State = Either GcdState GcdState

gcdEvents :: EventsSpec
gcdEvents = [ Event "setX"  (1, Nothing)
            , Event "setY"  (1, Nothing)
            , Event "calc"  (0, Nothing)
            , Event "combo" (1, Nothing)
            , Event "clear" (0, Nothing)
            ]
            

gcdErrEvent = Event "err" (0, Nothing)

genRInt :: Int -> IO Int
genRInt n = getStdRandom (randomR (1, n))

genGcdEvent :: Event -> IO Event            
genGcdEvent ev = case ev of
  Event "setX"  (ar,_) -> do x <- genRInt max_par_val
                             return (Event "setX" (ar, Just [x]))
  Event "setY"  (ar,_) -> do y <- genRInt max_par_val
                             return (Event "setY" (ar, Just [y]))
  Event "calc"  ps     -> return (Event "calc" ps)
  Event "combo" (ar,_) -> do t <- genRInt 3
                             return (Event "combo" (ar, Just [t]))
  Event "clear" ps     -> return (Event "clear" ps)
  
gcd' :: Int -> Int -> Int
gcd' x y | (x == 0) && (y == 0) = 0
         | otherwise            = gcd x y

semGcdEvent :: GcdState -> Event -> State
semGcdEvent st e =  
  case e of
    Event "setX" (_, Just [x]) -> Right st{getX = Just x}
    Event "setY" (_, Just [y]) -> Right st{getY = Just y}
    Event "calc" _             -> let x = getX st
                                      y = getY st
                                  in if (x == Nothing) || (y == Nothing)   
                                     then Left st
                                     else Right st{getR = Just (gcd' (fromJust x) (fromJust y))}
    Event "combo" _            -> Right st
    Event "clear" _            -> Right gcdInitState 
            
gcdInitState :: GcdState
gcdInitState = State { getX = Nothing
                     , getY = Nothing
                     , getR = Nothing
                     }

genGcdLog :: GcdState -> Int -> IO Log  
genGcdLog _  0 = return []
genGcdLog st l = do i   <- genRInt lenEs
                    e   <- genGcdEvent $ gcdEvents !! (i-1)
                    let st' = semGcdEvent st e
                    let logEntry = case st' of
                          Left  er_st -> [LogE e er_st, LogE gcdErrEvent st]
                          Right st''  -> [LogE e st] 
                    log <- genGcdLog (either id id st') (l-1)
                    return (logEntry ++ log)

  where
    lenEs = length gcdEvents

testGcdLog n = genGcdLog gcdInitState n
                
genGcdLogInFile :: Int -> IO ()
genGcdLogInFile n = testGcdLog n >>= (\l -> writeFile "gcdlog.out" (show l))

                