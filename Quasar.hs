
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


-- applyWithLog :: (generation, value, log)
applyWithLog :: (Show a, Show r) => Free (Universe (a->a)) r -> a -> (Int, a, String)
applyWithLog (Free (Era e next)) v =
    (cnt+1,
    (e v'),
    show cnt ++ "th era:" ++ show (e v') ++ "\n" ++ str)

    where
        (cnt, v', str) = applyWithLog next v

applyWithLog (Free BigCrunch) v =
    (1, v, "Big Crunch!\n")

apply :: Free (Universe (a->a)) r -> a -> a
apply (Free (Era e next)) v = e (apply next v)
apply (Free BigCrunch) v = v

showMaybeUniverse :: (Show a) => Maybe (Free (Universe (a->a)) ()) -> a -> String
showMaybeUniverse u v = case u of
    Just u' -> show $ u' `apply` v
    Nothing -> "Illegal Time/Space Travel"


-- Timemachine can go back to a past universe
mayBackTo :: Free (Universe a) () -> Int -> Maybe (Free (Universe a) ())
mayBackTo x 0 = Just x
mayBackTo (Free (Era e next)) n = mayBackTo next (n-1)
mayBackTo (Free BigCrunch) n = Nothing

backTo :: Free (Universe a) () -> Int -> Free (Universe a) ()
backTo u n = case u `mayBackTo` n of
    Just u' -> u'
    Nothing -> Free BigCrunch

-- Qubit contains many bits
type Qubit = []

bind :: Qubit a -> (a->b) -> Qubit b
bind = flip map

-- ParallelWorld is a set of Universe
type ParallelWorld a = [Free (Universe a) ()]

-- Universe Range
univRange :: (a->b) -> Free (Universe a) () -> Free (Universe b) ()
univRange _ _ = Free BigCrunch

-- QuantumComputer apply a value to all Universe in ParallelWorld
qComputer :: ParallelWorld (a->a) -> a -> [a]
qComputer p v = map (`apply` v) p

-- Blackhole is fixed
bHole :: Maybe a
bHole = Nothing

-- DarkMatter defines nothing
dMatter :: a
dMatter = undefined



anUniverse :: Free (Universe (Int->Int)) ()
anUniverse = do
    era (3+)
    era (2+)
    era (1+)
    bigCrunch

--main = putStrLn $ showMaybeUniverse (anUniverse `mayBackTo` 2) 6
main = putStrLn $ showMaybeUniverse (anUniverse `mayBackTo` 8) 6
--main = print $ [1,2,3,4] `bind` (+1)
--main = print $ qComputer [anUniverse, anUniverse `backTo` 1, anUniverse `backTo` 2] 2



