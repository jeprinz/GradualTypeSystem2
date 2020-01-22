module Lambda(
    Exp (Var, App, Lam, LAtomic, Let),
    AExp' (AVar, AApp, ALam, ALAtomic, ALet),
    AExp,
    applySubsAexp
) where

import Id
import Data.Map as Map
import Type


import NameGiver
import Control.Monad.State

data Exp = Var Id | App Exp Exp | Lam Id Exp | LAtomic Type String | Let Id Exp Exp deriving(Show)

data AExp' =
    AVar Id | AApp AExp AExp | ALam Id AExp |
    ALAtomic String | ALet Type Id AExp AExp deriving(Show)
    -- In ALet, the first Type is the type of the Variable, the second is of the whole expression. E.g,
    -- (let x : A = e1 in e2) : B is encoded (ALet A B x e1 e2)
type AExp = (Type, AExp')

data Bla' = Bloop Bla
type Bla = (Type, Bla')

-- typeOf :: AExp -> Type
-- typeOf 

type TypeState' = TypeState Id Char

aExp'ToStringI :: AExp' -> TypeState' String
aExp'ToStringI (AApp e1 e2) = do
    left <- aExpToStringI e1
    right <- aExpToStringI e2
    return (left ++ " " ++ right)
aExp'ToStringI (ALam x e) = do
    es <- aExpToStringI e
    varName <- getName x
    return ("l " ++ [varName] ++ ". " ++ es)
aExp'ToStringI (AVar x) = do
    varName <- getName x
    return $ [varName]
aExp'ToStringI (ALAtomic text) = do
    return $ text
aExp'ToStringI (ALet tx x e1 e2) = do -- TODO: how can it get a type for the variable x?
    varName <- getName x
    s1 <- aExpToStringI e1
    s2 <- aExpToStringI e2
    txs <- typeToStringI tx False
    return $ "let " ++ [varName] ++ " [" ++ txs ++  "] = " ++ s1 ++ " in " ++ s2

aExpToStringI :: AExp -> TypeState' String
aExpToStringI (t, aexp) = do
    ts <- typeToStringI t False
    s <- aExp'ToStringI aexp
    return $ "(" ++ s ++ ")[" ++ ts ++ "]"
    

aExpToString :: AExp -> String
aExpToString aexp = evalState (aExpToStringI aexp) (['A'..'Z'], Map.empty)

applySubsAexp :: Substitutions -> AExp -> AExp
applySubsAexp subs (t, aExp) =
    let t' = applySubs subs t
        aExp' = case aExp of
                      AVar x -> AVar x
                      AApp e1 e2 -> AApp (applySubsAexp subs e1) (applySubsAexp subs e2)
                      ALam x e -> ALam x (applySubsAexp subs e)
                      ALAtomic s -> ALAtomic s
                      ALet t x a1 a2 -> ALet (applySubs subs t) x (applySubsAexp subs a1) (applySubsAexp subs a2)
    in (t', aExp')

