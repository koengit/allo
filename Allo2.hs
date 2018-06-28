module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map( Map, (!) )
import Data.Char ( isAlphaNum )
import System.Process( system )
import System.IO 
import System.Environment
import Prelude hiding ( Word )

--------------------------------------------------------------------------------
-- main

main :: IO ()
main =
  do let (prob,vtab) = mkProblem corpus1 feat1
     sol <- cplex prob
     putStr $ unlines $
       [ f ++ ": " ++ p
       | v <- sol
       , let (f,p) = vtab!v
       ]

--------------------------------------------------------------------------------
-- types

type Feature    = String
type Word       = String
type Pattern    = Word
type Corpus     = [(Word,[Feature])]
type FeatureMap = [(Feature,[Pattern])]

corpus1 :: Corpus
corpus1 =
  [ ("bil",   ["car","sg"])
  , ("bilar", ["car","pl"])
  , ("duk",   ["cloth","sg"])
  , ("dukar", ["cloth","pl"])
  , ("böcker",   ["book","pl"])
  , ("bok",     ["book","sg"])
  , ("son", ["son", "sg"])
  , ("söner", ["son","pl"])
  , ("katt", ["cat", "sg"])
  , ("katter", ["cat", "pl"])
  , ("glas", ["glass","sg"])
  , ("glas", ["glass","pl"])
  , ("ämne", ["topic", "sg"])
  , ("ämnen", ["topic", "pl"])
  ]

feat1 :: FeatureMap
feat1 =
  [ ("car",   ["bil*","bila*"])
  , ("cloth", ["duk*","duka*"])
  , ("book",  ["bok*","böck*"])
  , ("son",   ["son*","sön*"])
  , ("cat",   ["katt*","kat*","katter*"])
  , ("glass", ["glas*"])
  , ("topic", ["ämne*"])
  , ("sg", ["*"])
  , ("pl", ["*","+er*","+ar*","+n*","+en*"])
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

corpus3 :: Corpus
corpus3 =
  [ ("word",   ["become","p1sg"])
  , ("wordt",  ["become","p2sg"])
  , ("wordt",  ["become","p3sg"])
  , ("worden", ["become","p2pl"])
  , ("geworden",["become","pastp"])
  , ("hoor",   ["hear","p1sg"])
  , ("hoort",  ["hear","p2sg"])
  , ("hoort",  ["hear","p3sg"])
  , ("horen",  ["hear","p1pl"])
  , ("gehoord",["hear","pastp"])
  , ("werk",   ["work","p1sg"])
  , ("werkt",  ["work","p2sg"])
  , ("werkt",  ["work","p3sg"])
  , ("werken", ["work","p1pl"])
  , ("gewerkt",["work","pastp"])
  , ("ga",    ["go","p1sg"])
  , ("gaat",  ["go","p2sg"])
  , ("gaat",  ["go","p3sg"])
  , ("gaan",  ["go","p1pl"])
  , ("gegaan",["go","pastp"])
  ]

--------------------------------------------------------------------------------
-- problem (to be solved by an LP solver)

type Name   = String
type Term   = (Integer,Name)
data Constr = [Term] :=: Integer | [Term] :>=: Integer deriving ( Show, Ord, Eq )

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

cost :: Feature -> Pattern -> Int
cost f p = 1
--cost f p = 1000-length p

mkProblem :: Corpus -> FeatureMap -> (Problem, Map Name (Feature,Pattern))
mkProblem corp fmap = (problem, vtab)
 where
  problem =
    Problem
    { minimise = [ (fromIntegral (cost f p), v)
                 | (f,pvs) <- M.toList ftab
                 , (p,v) <- pvs
                 ]
    , constrs  = [ c
                 | ((w,fs),i) <- corp `zip` [0..]
                 , c <- constrsFor i w fs
                 ]
    }

  safe s = map (\c -> if isAlphaNum c then c else '_') s
  ftab   = M.fromList [ ( feat
                        , [ (p, "sol_"
                             ++ show i
                             ++ "_"
                             ++ safe feat
                             ++ "_"
                             ++ safe p
                             ++ show j)
                          | (p,j) <- pats `zip` [0..]
                          ]
                        )
                      | ((feat,pats),i) <- fmap `zip` [0..]
                      ]

  vtab =
    M.fromList
    [ (v,(feat,p))
    | (feat,pvs) <- M.toList ftab
    , (p,v) <- pvs
    ]

  constrsFor c w fs =
    noErrors $
    -- exactly one pattern is used for each feature
    [ [ (1,m i j)
      | ((p,_,ls),j) <- pvls `zip` [0..]
      ] :=: 1
    | (pvls,i) <- pats `zip` [0..]
    ] ++

    -- exactly one pattern for each letter l
    nub
    [ [ (1,m i j)
      | (pvls,i) <- pats `zip` [0..]
      , ((p,_,ls),j) <- pvls `zip` [0..]
      , l `elem` ls
      ] :=: 1
    | l <- [0..length w-1]
    ] ++

    -- a pattern match in the word triggers that allomorf
    [ ( (1,v)
      : [ (-1,m i j)
        | ((_,w,_),j) <- pvls `zip` [0..]
        , w == v
        ]
      ) :>=: 0
    | (pvls,i) <- pats `zip` [0..]
    , v <- nub [ v | (_,v,_) <- pvls ]
    ]
   where
    -- "the ith feature matches its jth pattern"
    m i j = "m" ++ show c ++ "_" ++ show i ++ "_" ++ show j
    
    -- all patterns that can be applied anywhere in the word
    pats = [ [ (p,v,ls) | (p,v) <- ftab!f, ls <- matches p w ]
           | f <- fs
           ]

    -- sanity checks for understandable output
    errors =
      [ "feature <" ++ f ++ "> cannot be used to explain word <" ++ w ++ ">"
      | (pvls,f) <- pats `zip` fs
      , null pvls
      ] ++
      [ "letter #" ++ show l ++ " of word <" ++ w ++ "> (" ++ show (w!!l)
        ++ ") cannot be explained by any of its features"
      | l <- [0..length w-1]
      , null [ ()
             | (pvls,i) <- pats `zip` [0..]
             , ((p,_,ls),j) <- pvls `zip` [0..]
             , l `elem` ls
             ]
      ]

    noErrors x
      | null errors = x
      | otherwise   = error (head errors)

--------------------------------------------------------------------------------
-- solving a problem with cplex

cplex :: Problem -> IO [String]
cplex p =
  do putStrLn "+++ Generating input file..."
     writeFile "input.lp" $ unlines $
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
     putStrLn "+++ Starting CPLEX..."
     system "./cplex-ssh input.lp > solution.out"
     putStrLn "+++ Reading solution..."
     s <- readFile "solution.out"
     return [ v
            | l <- lines s
            , [v,one] <- [words l]
            , take 4 v == "sol_"
            , take 2 one == "1."
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

usort :: Ord a => [a] -> [a]
usort = S.toList . S.fromList

nub :: Ord a => [a] -> [a]
nub xs = go S.empty xs
 where
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs
  go seen []            = []

--------------------------------------------------------------------------------

matches :: Pattern -> Word -> [[Int]]
matches pat word = nub (go [(pat,[])] 0 word)
 where
  go pats i [] =
    [ reverse is
    | (p,is) <- pats
    , eps p
    ]

  go pats i (c:cs) =
    go
    [ (pat', [ i | b ]++is)
    | (pat,is) <- pats
    , (pat',b) <- eat c pat
    ]
    (i+1) cs

  eat c ""        = []
  eat c ('+':pat) = [('*':pat,False)]
  eat c ('*':pat) = [('*':pat,False)] ++ eat c pat
  eat c (a  :pat) = [(pat,True) | a == c]

  eps ""       = True
  eps ('*':as) = eps as
  eps _        = False

remove :: [Int] -> Word -> Word
remove is s = simp [ if i `elem` is then '+' else c | (c,i) <- s `zip` [0..] ]
 where
  simp ('+':'+':s) = simp ('+':s)
  simp (c:s)       = c : simp s
  simp ""          = ""

apply :: Pattern -> Word -> [Word]
apply pat word = nub [ remove is word | is <- matches pat word ]

applyTable :: [[Pattern]] -> Word -> [Word]
applyTable [] word = [word]
applyTable (pats:patss) word =
  [ word2
  | pat <- pats
  , word1 <- apply pat word
  , word2 <- applyTable patss word1
  ]

