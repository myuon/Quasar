{-# LANGUAGE TemplateHaskell #-}

module Universe where
import Language.Haskell.TH
import Control.Monad
import Control.Category
import Prelude hiding (id)

data Universe a b = a :> b | BigCrunch deriving (Show)

era :: (->) a b -> (->) a b
era = id

bigCrunch :: (->) a a
bigCrunch = id

-- eg. [ConP :> [VarP b_3,ConP :> [VarP a_4,ConP BigCrunch []]]]
univInfixP :: [Name] -> PatQ
univInfixP [] = undefined
univInfixP (x:[]) = conP '(:>) [varP x, conP 'BigCrunch []]
univInfixP xs = conP '(:>) [univInfixP' xs, conP 'BigCrunch []]
  where
    univInfixP' :: [Name] -> PatQ
    univInfixP' (x:y:[]) = conP '(:>) [varP x, varP y]
    univInfixP' xs = conP '(:>) [univInfixP' $ init xs, varP $ last xs]

-- eg. (AppE (AppE (VarE <<<) (AppE (AppE (VarE <<<) (VarE a_12)) (VarE b_13))) (VarE c_14))
univInfixE :: [Name] -> ExpQ
univInfixE [] = undefined
univInfixE (x:[]) = appE (appE (varE '(<<<)) (varE x)) (varE 'id)
univInfixE xs = appE (appE (varE '(<<<)) (univInfixE' xs)) (varE 'id)
  where
    univInfixE' :: [Name] -> ExpQ
    univInfixE' (x:y:[]) = appE (appE (varE '(<<<)) (varE x)) (varE y)
    univInfixE' xs = appE (appE (varE '(<<<)) (univInfixE' $ init xs)) (varE $ last xs)
                               
getNewNames :: Int -> Q [Name]
getNewNames n = replicateM n $ newName "x"

makeComposite :: Int -> ExpQ
makeComposite n = do
  name <- getNewNames n
  lamE [univInfixP name] (univInfixE name)


