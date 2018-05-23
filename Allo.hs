module Allo where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List ( inits )
import System.Process( system )

--------------------------------------------------------------------------------
-- main

main :: IO ()
main =
  do ftab <- cplex (corpusToProblem corpus2)
     putStr $ unlines $ map show $ M.toList ftab

--------------------------------------------------------------------------------
-- corpus (table of word + feature list)

type Feature = String
type Corpus  = [(String,[Feature])]

corpus1 :: Corpus
corpus1 =
  [ ("bil",   ["car","sg"])
  , ("bilar", ["car","pl"])
  , ("duk",   ["cloth","sg"])
  , ("dukar", ["cloth","pl"])
  ]

corpus2 :: Corpus
corpus2 =
  [ ("gehen",    ["walk","inf"])
  , ("gegangen", ["walk","pastperfect"])
  , ("sehen",    ["see","inf"])
  , ("gesehen",  ["see","pastperfect"])
  , ("machen",   ["do","inf"])
  , ("gemacht",  ["do","pastperfect"])
  , ("laufen",   ["run","inf"])
  , ("gelaufen", ["run","pastperfect"])
  , ("kaufen",   ["buy","inf"])
  , ("gekauft",  ["buy","pastperfect"])
  ]

--------------------------------------------------------------------------------
-- problem (to be solved by an LP solver)

type Name   = String
type Term   = (Integer,String)
data Constr = [Term] :=: Integer | [Term] :>=: Integer deriving ( Show )

lhs :: Constr -> [Term]
lhs (axs :>=: c) = axs
lhs (axs :=:  c) = axs

data Problem
  = Problem
  { minimise :: [Term]
  , constrs  :: [Constr]
  }

--------------------------------------------------------------------------------
-- corpus -> problem

corpusToProblem :: Corpus -> Problem
corpusToProblem corp =
  Problem
  { minimise = [ (1,f `allo` s)
               | (f,ss) <- M.toList featMap
               , s <- S.toList ss
               ]
  , constrs  = [ c
               | ((w,fs),i) <- corp `zip` [1..]
               , c <- constrsFor i w fs
               ]
  }
 where
  featMap = M.fromListWith S.union
            [ (f,S.singleton s)
            | (w,fs) <- corp
            , s <- subs w
            , f <- fs
            ]

  constrsFor i w fs =
    -- exactly one part for each letter l
    [ [ (1,p) | (p,ls) <- parts `zip` subs w', l `elem` ls ] :=: 1
    | l <- w'
    ] ++
    -- if a part exists, at least one feature must explain it
    [ ((-1,p) : [ (1,f `allo` s) | f <- fs ]) :>=: 0
    | (p,s) <- parts `zip` subs w
    ] ++
    -- every feature must explain some part of the word (can be the empty string as well)
    concat
    [ [ [ (1,p ++ "_" ++ f) | p <- parts ] :>=: 1 ] ++
      [ [ (-1,p ++ "_" ++ f), (1,f `allo` s) ] :>=: 0
      | (p,s) <- parts `zip` subs w
      ] ++
      [ [ (-1,p ++ "_" ++ f), (1,p) ] :>=: 0
      | (p,s) <- parts `zip` subs w
      ]
    | f <- fs
    ]
   where
    -- the word, but every letter is now unique
    w' = map snd (w `zip` [1..])
    
    -- a unique name for every possible partition of the word
    parts = [ "w" ++ show i ++ "_" ++ w ++ "_" ++ show j ++ "_" ++ s
            | (s,j) <- subs w `zip` [1..]
            ]

allo :: Feature -> String -> Name
f `allo` s = "allo_" ++ f ++ "_" ++ s

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = [ x:ys | ys <- inits xs ] ++ subs xs

--------------------------------------------------------------------------------
-- solving a problem with cplex, and parsing the solution

cplex :: Problem -> IO (M.Map Feature [String])
cplex p =
  do writeFile "input.lp" $ unlines $
       [ "MINIMIZE"
       , "  " ++ showTerms (minimise p)
       , "SUBJECT TO"
       ] ++
       [ "  " ++ showConstr c
       | c <- constrs p
       ] ++
       [ "BINARY"
       ] ++
       [ "  " ++ v
       | v <- vs
       ] ++
       [ "END"
       ]
     writeFile "script.in" $ unlines $
       [ "r input.lp"
       , "opt"
       , "display solution variables *"
       , "quit"
       ]
     system "cplex < script.in > solution.out"
     s <- readFile "solution.out"
     return $ M.fromListWith (++)
       [ (takeWhile (/= '_') v', [drop 1 (dropWhile (/= '_') v')])
       | l <- lines s
       , take 5 l == "allo_"
       , [v,one] <- [words l]
       , read one == (1.0 :: Double)
       , let v' = drop 5 v
       ]
 where
  vs = S.toList $ S.fromList $
       [ x | (_,x) <- minimise p ] ++
       [ x | c <- constrs p, (_,x) <- lhs c ]

showTerms :: [Term] -> String
showTerms []  = "0"
showTerms axs = noPlus (unwords [ show' a x | (a,x) <- axs, a /= 0 ])
 where
  noPlus ('+':' ':s) = s
  noPlus ('+':s)     = s
  noPlus s           = s
  
  show' 1    x         = "+ " ++ x
  show' (-1) x         = "- " ++ x
  show' a    x | a > 0 = "+ " ++ show a ++ " " ++ x
  show' a    x | a < 0 = "- " ++ show (-a) ++ " " ++ x

showConstr :: Constr -> String
showConstr (axs :>=: c) = showTerms axs ++ " >= " ++ show c
showConstr (axs :=:  c) = showTerms axs ++ " = "  ++ show c

--------------------------------------------------------------------------------

