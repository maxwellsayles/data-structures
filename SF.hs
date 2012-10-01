{-|
Example of the Stream Function arrow.  This is experimental and should
be cleaned up.
-}

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

newtype SF a b = SF { runSF :: [a] -> [b] }

instance Category SF where
    id = SF id
    (SF f) . (SF g) = SF (f . g)

instance Arrow SF where
    arr f = SF (map f)
    first (SF f) = SF $ unzip >>> first f >>> uncurry zip

instance ArrowChoice SF where
    left (SF f) = SF $ \xs -> combine xs (f [l | Left l <- xs])
        where combine []              _     = []
              combine (Left  x : xs) (z:zs) = (Left  z) : combine xs zs
              combine (Right x : xs)  zss   = (Right x) : combine xs zss

delay x = SF (init . (x:))

assoc ((a,b),c) = (a,(b,c))
unassoc (a,(b,c)) = ((a,b),c)

listcase []     = Left []
listcase (x:xs) = Right (x, xs)

boolcase  True = arr Right
boolcase False = arr Left

mapA f = listcase ^>> arr id ||| (f *** mapA f >>^ uncurry (:))


filterA f = listcase ^>> arr id ||| ((f &&& arr id) *** filterA f >>^ cons)
    where cons ((False, _), xs) = xs
          cons ((True, x), xs) = x:xs

filterA' f = listcase ^>> arr id ||| (helper >>^ snd ||| uncurry (:))
    where helper = (((f >>^ boolcase) &&& arr id) *** filterA' f) >>^ assoc >>> app

f x = print x >> return (odd x)

main = do
--  let f x = print x >> return (show x)
--  let xs = map (\x -> if odd x then Left x else Right x) [1..100]
--  out <- runKleisli (mapA (Kleisli f +++ arr pred)) xs
--  print out
--
--  print =<< runKleisli (mapA     (Kleisli f)) [1..10]
--  print =<< runKleisli (filterA  (Kleisli f)) [1..10]
--  print =<< runKleisli (filterA' (Kleisli f)) [1..10]
--
  print $ runSF (arr (*2) &&& arr succ) [1..5]