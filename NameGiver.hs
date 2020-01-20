module NameGiver(
    TypeState,
    unique,
    getMap,
    putMap,
    getName
)where

import Control.Monad.State
import Data.Map as Map

type TypeState a b = State ([b], Map a b)

getName :: (Ord a) => a -> TypeState a b b
getName v = do vars <- getMap
               case Map.lookup v vars of
                    Just name -> return name
                    Nothing -> do u <- unique
                                  putMap (Map.insert v u vars)
                                  return u

unique :: TypeState a b b
unique = do (us, vars) <- get
            put (tail us, vars)
            return (head us)

getMap :: TypeState a b (Map a b)
getMap = do (us, vars) <- get
            return vars

putMap :: Map a b-> TypeState a b ()
putMap newMap = do (us, _) <- get
                   put (us, newMap)
                   return ()

    -- case Map.lookup v vars of
    -- Just s -> return s
    -- Nothing -> unique
