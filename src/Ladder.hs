module Ladder
  ( Dictionary,
    mkLadderGraph,
    readDictionary,
  )
where

import qualified Data.List as L
import qualified Graph as G
import qualified PermutationMap as PM

type Dictionary = [String]

readDictionary :: FilePath -> IO Dictionary
readDictionary filepath = do
  dictionaryContent <- readFile filepath
  let
    fileLines = L.lines dictionaryContent
    fileWords = L.map (L.filter (`L.elem` ['a' .. 'z'])) fileLines
  return (L.nub fileWords)

mkLadderGraph :: Dictionary -> G.DiGraph String
mkLadderGraph dict = G.buildDiGraph nodes
  where
    permutationMap = PM.createPermutationMap dict
    nodes =
      L.map (\w -> (w, computeCandidates permutationMap w)) dict

computeCandidates :: PM.PermutationMap -> String -> [String]
computeCandidates permutationMap word =
  let candidates = modified ++ removed ++ added ++ [word]
      uniques = L.nub [L.sort w | w <- candidates]
      perms = L.concatMap (\x -> PM.findWithDefault [] x permutationMap) uniques
   in L.delete word perms
  where
    added = [x : word | x <- ['a' .. 'z']]
    removed = [L.delete x word | x <- word]
    modified =
      [x : L.delete y word | x <- ['a' .. 'z'], y <- word, x /= y]

