import qualified Data.Map as Map
import Data.Map (Map)
import Prelude hiding (min)

data MinStack k v = MinStack (Map k v) (Map k v)
                  deriving Show

empty = MinStack Map.empty Map.empty

push v (MinStack valToTime timeToVal) = 
    let time = if Map.null timeToVal
               then 0
               else (+) 1 . fst . Map.findMax $ timeToVal
        valToTime' = Map.insert v time valToTime
        timeToVal' = Map.insert time v timeToVal
    in  MinStack valToTime' timeToVal'


pop (MinStack valToTime timeToVal)
    | Map.null valToTime = error "pop on empty stack"
    | otherwise = let (t, v) = Map.findMax timeToVal
                      valToTime' = Map.delete v valToTime
                      timeToVal' = Map.delete t timeToVal
                  in  MinStack valToTime' timeToVal'

top (MinStack _ timeToVal)
    | Map.null timeToVal = error "top on empty stack"
    | otherwise = snd $ Map.findMax timeToVal

min (MinStack valToTime _)
    | Map.null valToTime = error "min on empty stack"
    | otherwise = fst $ Map.findMin valToTime

deleteMin (MinStack valToTime timeToVal)
    | Map.null valToTime = error "deleteMin on empty stack"
    | otherwise = let (v, t) = Map.findMin valToTime
                      valToTime' = Map.delete v valToTime
                      timeToVal' = Map.delete t timeToVal
                  in  MinStack valToTime' timeToVal'

main =
    do let s = push 6 $ push 5 $ empty
       print s
       print $ top s
       print $ min s