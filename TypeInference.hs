-- This file has not been updated to GradualTypeInference version 2
module TypeInference(
  infer
) where

import Lambda
import Id
import Type
import Data.Map as Map

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Unique

import Debug.Trace

type FreeVars = Map LId Type

infer :: Exp -> Maybe (Type, FreeVars)
infer e = let (res, _) = withUniques (runMaybeT (inferI e)) (Prelude.map Id [0..])
          in res

inferI :: Exp -> MaybeT (WithUnique Id) (Type, FreeVars)
inferI (Lambda.Var x) =
  do t <- lift unique
     let var = Type.Var t
     return (var, Map.singleton x var)
inferI (Lam x e) =
  do (eType, free) <- inferI e
     let free' = delete x free
     xType <- case Map.lookup x free of
       Just t -> return t
       Nothing -> do i <- lift unique
                     return (Type.Var i)
     return (Fun xType eType, free')
inferI (App e1 e2) =
  do (e1Type, free1) <- inferI e1
     (e2Type, free2) <- inferI e2

     let (free, subs) = combineFreeVars free1 free2

     let e1Type' = applySubs subs e1Type
     let e2Type' = applySubs subs e2Type

     i <- lift unique
     let tau = Type.Var i
     (_, subs2) <- (MaybeT . return) $ intersect e1Type' (Fun e2Type' tau) -- MaybeT . return is magic https://stackoverflow.com/questions/8684252/how-to-inject-a-maybe-value-into-maybet

     let free' = Map.map (applySubs subs2) free
     let tau' = applySubs subs2 tau

     return (tau', free')
inferI (LAtomic t) = return (t, Map.empty)

-- problem: returns wrong when given [x -> A], [x -> B] -- note to self: is this still relevant?
combineFreeVars :: FreeVars -> FreeVars -> (FreeVars, Substitutions)
combineFreeVars free1 free2 =
  let only1 = difference free1 free2
      only2 = difference free2 free1
      both = intersectionWith (\t1 t2 -> (t1, t2)) free1 free2 :: Map LId (Type, Type)
      (bothIds, bothTypes) = unzip (toList both :: [(LId, (Type, Type))])

      (intersectedTypes, subs) = unionList bothTypes
      combined = zip bothIds intersectedTypes

      only1Subbed = Map.map (applySubs subs) only1
      only2Subbed = Map.map (applySubs subs) only2
      bothCombined = fromList combined

      result = unions [only1Subbed, only2Subbed, bothCombined]
  in (result, subs)

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
