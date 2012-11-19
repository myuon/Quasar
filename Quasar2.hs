{-# LANGUAGE TemplateHaskell #-}

import Control.Category
import Prelude hiding (id)

import Universe

anUniverse = era show :> era (+8) :> era (*3) :> era (+1.5) :> BigCrunch

main = putStrLn $ $(makeComposite 4) anUniverse $ 2

