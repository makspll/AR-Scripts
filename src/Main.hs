import System.Directory.Internal.Prelude (getArgs, fromMaybe)
import Data.List.Split ( splitOn )
import Common ( Option(Unify,Match, CriticalPairs, Error, Normalize), parser, Rule ((:=>:)), RuleOccurrence, Exp, getRule, getPos, Critical ((:<>:)), Sub, getMGU)
import Unification(unify)
import Matching(match)
import Text.Printf (printf)
import Rewriting (criticalPairs, allCriticalPairs, reduceToNormal, applyRule)
import Data.Set (toList, fromList)
import qualified Data.Set as Data



indent :: String -> Int -> String
indent s i = foldl (\acc n -> if n == '\n' then acc ++ "\n" ++ replicate i '\t'  else acc ++ [n]) "" s

processMGU :: Data.Set Sub -> String
processMGU = show.toList

processMGUMaybe :: Maybe (Data.Set Sub) -> String
processMGUMaybe = processMGU.fromMaybe  (fromList [])

processCritical :: [(Critical, (Rule,Rule),Data.Set Sub)] -> String
processCritical xs = let


                    processCritical' :: (Critical, (Rule,Rule),Data.Set Sub) -> String
                    processCritical' (p,(rule1@(l1 :=>: r1),rule2@(l2 :=>: r2)),mgu) = printf "\nPair:{%s}\nAssignments:\n{\n\tWhole Rule:%s\n\tSub Rule:%s\n\tMGU:%s\n}" (show p) (show rule1) (show rule2) (processMGU mgu)
                    in concatMap processCritical' xs



processNormal :: Exp -> [(Exp, [RuleOccurrence ])] -> String
processNormal original xs = let

                    folder :: (String,Exp)-> RuleOccurrence -> (String,Exp)
                    folder (soFar, lastExp) nRuleOccurence
                        | getPos nRuleOccurence == -1 = (soFar,lastExp)
                        | otherwise =  let
                            reduced = applyRule nRuleOccurence lastExp
                            in (soFar ++ "\n\t"++ show lastExp ++ " > " ++ show reduced ++ printf " with rule: %s, applied at pos %s, MGU:%s" (show $ getRule nRuleOccurence) (show $ getPos nRuleOccurence) (processMGU $ getMGU nRuleOccurence) ,reduced)

                    processNormal' :: (Exp, [RuleOccurrence]) -> String
                    processNormal' (e,rules) = printf "\nForm:{%s}\nSteps:\n{%s\n}" (show e) $ fst (foldl folder ("",original) rules)
                    in concatMap processNormal' xs

process :: Option -> String
process (Unify a b) = printf "Unify {%s , %s}: %s \n" (show a) (show b) (processMGUMaybe (unify a b))
process (Match a b) = printf "Match {%s -> %s}: %s \n" (show a) (show b) (processMGUMaybe (match a b))
process (CriticalPairs xs) =
    let
        args =  foldl (\acc x -> acc ++ "\n" ++ show x) "" xs
        out =  "["  ++ indent (processCritical (allCriticalPairs xs)) 2 ++ "\n]"
    in printf "CriticalPairs {%s \n}%s\n" (indent args 1 ) out
process (Normalize xs e) =
    let
        args =  foldl (\acc x -> acc ++ "\n" ++ show x) "" xs
        out = "["++ indent ( processNormal e (reduceToNormal xs e)) 2 ++ "\n]"
    in printf "NormalForms {%s\n\n\tapplied to %s\n}%s\n" (indent args 1) (indent (show e) 1) out
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
