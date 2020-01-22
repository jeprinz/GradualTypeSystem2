module Main where

import Id
import LambdaParse
import TypeInference
import Data.Map as Map
import Lambda
import Data.List as List
import Type

import System.IO

main :: IO ()
main = do takeLine
          main

takeLine :: IO ()
takeLine = do putStr ">> "
              hFlush stdout
              line <- getLine
              let (e, varNames) = lambdaParseInfo line
              case infer e of
                Just ((t, aexp), free) ->
                  do let (lIds, types) = unzip (toList free)
                    --  let (asString : freeVarTypes) = typesToStrings (t : types)
                     let asString = aExpToString (t, aexp)
                     putStrLn asString
                    --  putStrLn (freeVarInfo lIds varNames freeVarTypes)
                Nothing -> putStrLn "Conflicting types"
-- the old one that only outputs type
-- takeLine :: IO ()
-- takeLine = do putStr ">> "
--               hFlush stdout
--               line <- getLine
--               let (e, varNames) = lambdaParseInfo line
--               case infer e of
--                 Just ((t, aexp), free) ->
--                   do let (lIds, types) = unzip (toList free)
--                     --  let (asString : freeVarTypes) = typesToStrings (t : types)
--                      let asString = typeToString t
--                      putStrLn asString
--                     --  putStrLn (freeVarInfo lIds varNames freeVarTypes)
--                 Nothing -> putStrLn "Conflicting types"

freeVarInfo :: [Id] -> (Map Id String) -> [String] -> String
freeVarInfo lIds nameMap types =
          let maybenames = Prelude.map ((flip Map.lookup) nameMap) lIds
              names = Prelude.map (\mn -> case mn of
                        Just s -> s
                        Nothing -> "Error") maybenames
              namesTypes = zip names types :: [(String, String)]
              lines = Prelude.map (\(var, t) -> var ++ ": " ++ t) namesTypes
          in intercalate "\n" lines