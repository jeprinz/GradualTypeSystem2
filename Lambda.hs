module Lambda(
    Exp (Var, App, Lam, LAtomic, Let)
) where

import Data.Map as Map
import Id
import Type
import NameGiver
import Control.Monad.State

data Exp = Var Id | App Exp Exp | Lam Id Exp | LAtomic Type String | Let Id Exp Exp deriving(Show)

data AExp =
    AVar Type Id | AApp Type AExp AExp | ALam Type Id AExp |
    ALAtomic Type String | ALet Type Type Id AExp AExp deriving(Show)
    -- In ALet, the first Type is the type of the Variable, the second is of the whole expression. E.g,
    -- (let x : A = e1 in e2) : B is encoded (ALet A B x e1 e2)

type TypeState' = TypeState Id Char

aExpToStringI :: AExp -> TypeState' String
aExpToStringI (AApp t e1 e2) = do
    left <- aExpToStringI e1
    right <- aExpToStringI e2
    ts <- typeToStringI t False
    return ("(" ++ left ++ right ++ " )[" ++ ts ++ "]")
aExpToStringI (ALam t x e) = do
    es <- aExpToStringI e
    varName <- getName x
    ts <- typeToStringI t False
    return ("(l " ++ [varName] ++ ". " ++ es ++ " )[" ++ ts  ++ "]")
aExpToStringI (AVar t x) = do
    varName <- getName x
    ts <- typeToStringI t False
    return $ "( " ++ [varName] ++ " )[" ++ ts ++ "]"
aExpToStringI (ALAtomic t text) = do
    ts <- typeToStringI t False
    return $ "( " ++ text ++ " )[" ++ ts ++ "]"
aExpToStringI (ALet tx t x e1 e2) = do -- TODO: how can it get a type for the variable x?
    varName <- getName x
    s1 <- aExpToStringI e1
    s2 <- aExpToStringI e2
    ts <- typeToStringI t False
    txs <- typeToStringI tx False
    return $ "(let " ++ [varName] ++ " [" ++ txs ++  "] = " ++ s1 ++ " in " ++ s2 ++ ")[" ++ ts ++ "]"
    


aExpToString :: AExp -> String
aExpToString aexp = evalState (aExpToStringI aexp) (['A'..'Z'], Map.empty)
