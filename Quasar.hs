import qualified Data.Set
import Control.Monad.Free

-- Multiverse contains universes
data Multiverse a = Universe a | Multiverse

-- Universe contains Eras and Big Crunch
data Universe e next = Era e next | BigCrunch

instance Functor (Universe anEra) where
    fmap f (Era e next) = Era e (f next)
    fmap f BigCrunch = BigCrunch

era x = liftF (Era x ())
bigCrunch = liftF BigCrunch

-- Readerとして使っている
-- showUniverse _ :: (世代, 値, 途中経過を示す文字列)
showUniverse :: (Show a, Show r) => Free (Universe (a->a)) r -> a -> (Int, a, String)
showUniverse (Free (Era e next)) v =
    (cnt+1, (e v'), show cnt ++ "th era:" ++ show (e v') ++ "\n" ++ str)
        where (cnt, v', str) = showUniverse next v
showUniverse (Free BigCrunch) v =
    (1, v, "Big Crunch!\n")


-- Timemachine
backTo :: Int -> Free (Universe (Int->Int)) () -> Free (Universe (Int->Int)) ()
backTo 0 x = x
backTo n (Free (Era e next)) = backTo (n-1) next
backTo n (Free BigCrunch) = Free BigCrunch

anUniverse :: Free (Universe (Int->Int)) ()
anUniverse = do
    era (3+)
    era (2+)
    era (1+)
    bigCrunch

main = putStr $ (\(x,y,z)->z) $ showUniverse (backTo 9 anUniverse) $ 1

