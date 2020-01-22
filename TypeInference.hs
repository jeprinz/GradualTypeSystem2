module TypeInference(
  infer
) where

import Lambda
import Id
import Type
import Data.Map as Map

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Foldable
import Unique

import Debug.Trace

type FreeVars = Map Id [Type]-- these lists of types will be nonempty, could use nonempty list type

applySubsToFree :: FreeVars -> Substitutions -> FreeVars
applySubsToFree free subs = Map.map (Prelude.map (applySubs subs)) free

infer :: Exp -> Maybe (AExp, FreeVars)
infer e = let (res, _) = withUniques (runMaybeT (inferI e)) (Prelude.map Id [0..])
          in res

inferI :: Exp -> MaybeT (WithUnique Id) (AExp, FreeVars)
inferI (Lambda.Var x) =
  do t <- lift unique
     let var = Type.Var t
     return ((var, AVar x), Map.singleton x [var])
inferI (Lam x e) =
  do (ae, free) <- inferI e
     let (eType, _) = ae
     (xType, subs) <- case Map.lookup x free of
       Just t -> return (combineList t)
       Nothing -> do i <- lift unique
                     return (Type.Var i, [])
     let eType' = applySubs subs eType
     let free' = applySubsToFree (delete x free) subs
     let ae' = applySubsAexp subs ae
     return ((Fun xType eType', ALam x ae'), free')
inferI (App e1 e2) =
  do (ae1, free1) <- inferI e1
     (ae2, free2) <- inferI e2
     let (e1Type, _) = ae1
     let (e2Type, _) = ae2

     let free = combineFreeVars free1 free2


     i <- lift unique
     let tau = Type.Var i
     (_, subs) <- (MaybeT . return) $ intersect e1Type (Fun e2Type tau) -- MaybeT . return is magic https://stackoverflow.com/questions/8684252/how-to-inject-a-maybe-value-into-maybet

     let free' = applySubsToFree free subs
     let tau' = applySubs subs tau
     let ae1' = applySubsAexp subs ae1
     let ae2' = applySubsAexp subs ae2

     return ((tau', AApp ae1 ae2), free')
inferI (LAtomic t text) = return ((t, ALAtomic text), Map.empty)
inferI (Let x e1 e2) =
  do  (ae1, free1) <- inferI e1
      (ae2, free2) <- inferI e2
      let te1 = fst ae1
      let free = combineFreeVars free1 free2
      let subs = case Map.lookup x free2 of
                   Just ts -> let onEach t = do (_, subs) <- Type.intersect te1 t
                                                return subs
                                  mSubsList = Prelude.map onEach ts :: [Maybe Substitutions]
                                  subsList = Prelude.map (\ms -> case ms of
                                                                   Just subs -> subs
                                                                   Nothing -> []) mSubsList
                                  subs = foldr1 (++) subsList -- if this line crashes, it means that the FreeVars map broke the rule of no empty lists
                              in  subs :: Substitutions
                   Nothing -> []
      let free' = delete x free
      let ae1' = applySubsAexp subs ae1
      let ae2' = applySubsAexp subs ae2
      let t = fst ae2'
      return ((t, ALet t x ae1' ae2'), free')

combineList :: [Type] -> (Type, Substitutions)
combineList [] = error "the list shouldn't have been empty"
combineList [t] = (t, [])
combineList (t : ts) = let (typeSoFar, subsSoFar) = combineList ts
                           (resultType, subs) = Type.union t typeSoFar
                       in  (resultType, subsSoFar ++ subs)
-- is this union or intersection? whichever one used to be in combineFreeVars. union!

combineFreeVars :: FreeVars -> FreeVars -> FreeVars
combineFreeVars free1 free2 =
  let only1 = difference free1 free2
      only2 = difference free2 free1
      both = intersectionWith (++) free1 free2

  in  unions [only1, only2, both]

unionList :: [(Type, Type)] -> ([Type], Substitutions)
unionList [] = ([], [])
unionList ((t1,t2) :  ts) = let (t, subs) = Type.union t1 t2
                                (rest, subs2) = unionList ts
                                subbedRest = Prelude.map (applySubs subs) rest
                            in (t : subbedRest, subs ++ subs2)


-- Instead of just storing Type, store Substitutions -> Type
-- and then always evaluate with the current substitutions.

newtype WithSubs = WithSubs (Substitutions -> Type)

-- operate inside of a (State Substitutions) monad, and use get to
getType :: WithSubs -> State Substitutions Type
getType (WithSubs withSubs) = do subs <- get
                                 return (withSubs subs)
