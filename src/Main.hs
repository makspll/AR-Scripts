import System.Directory.Internal.Prelude (getArgs)
import Data.List.Split

import Parser ( Option(Unify,Match), parser)
import Unification(unify)
import Matching(match)
import Text.Printf (printf)


process :: Option -> String
process (Unify a b) = printf "Unify <%s , %s>: %s" (show a) (show b) (show (unify a b))
process (Match a b) = printf "Match <%s -> %s>: %s" (show a) (show b) (show (match a b))

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    let lines = splitOn "\n" content
    let tasks = fmap (process.fst.head.parser) lines
    mapM_ putStrLn tasks
