import System.Directory.Internal.Prelude (getArgs, fromMaybe)
import Data.List.Split ( splitOn )
import Common ( Option(Unify,Match, CriticalPairs, Error, Normalize), parser)
import Unification(unify)
import Matching(match)
import Text.Printf (printf)
import Rewriting (criticalPairs, allCriticalPairs, reduceToNormal)
import Data.Set (toList, fromList)


process :: Option -> String
process (Unify a b) = printf "Unify {%s , %s}: %s \n" (show a) (show b) (show.toList $ fromMaybe  (fromList []) (unify a b))
process (Match a b) = printf "Match {%s -> %s}: %s \n" (show a) (show b) (show.toList $ fromMaybe  (fromList []) (match a b))
process (CriticalPairs xs) =
    let
        args =  foldl (\acc x -> acc ++ "\n" ++ show x) "" xs
        out = foldl (\acc x -> acc ++ "\n" ++ show x ++",") "[" (allCriticalPairs xs) ++ "\n]"
    in printf "CriticalPairs {%s \n}: %s \n" args out
process (Normalize xs e) = 
    let
        args =  foldl (\acc x -> acc ++ "\n" ++ show x) "" xs
        out = foldl (\acc x -> acc ++ "\n" ++ show x ++",") "[" (reduceToNormal xs e) ++ "\n]"
    in printf "NormalForms {%s \n applied to \n %s }: %s \n" args (show e) out
process _ = "Error in parsing or wrong arguments"


processOptions :: [Option] -> String
processOptions os = let
    strings = map process os
    in foldl (\acc b -> acc ++ "\n" ++ b) "" strings

main :: IO ()
main = do
    _ <-putStrLn "Processing"
    args <- getArgs
    content <- readFile (head args)
    let a = parser content
    let tasks = case parser content of
                    [] -> []
                    t -> map (processOptions.fst) t

    mapM_ putStrLn tasks
