module Lambda(
    Exp (Var, App, Lam, LAtomic, Let)
) where

import Data.Map as Map
import Id
import Type
import NameGiver
import Control.Monad.State

data Exp = Var Id | App Exp Exp | Lam Id Exp | LAtomic Type | Let Id Exp Exp deriving(Show)

data AExp =
    AVar Type Id | AApp Type AExp AExp | ALam Type Id AExp | ALAtomic Type deriving(Show)

type TypeState' = TypeState Id Char

aExpToStringI :: AExp -> TypeState' String
aExpToStringI (AApp t e1 e2) = do
    left <- aExpToStringI e1
    right <- aExpToStringI e2
    ts <- typeToStringI t False
    return (show left ++ show right ++ " : " ++ ts)
aExpToStringI (ALam t x e) = do
    es <- aExpToStringI e
    varName <- getName x
    ts <- typeToStringI t False
    return ("l " ++ [varName] ++ ". " ++ es ++ " : " ++ ts)


aExpToString :: AExp -> String
aExpToString aexp = evalState (aExpToStringI aexp) (['A'..'Z'], Map.empty)
