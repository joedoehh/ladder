module Ladder
  ( Dictionary,
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
    lines = L.lines dictionaryContent
    words = L.map (L.filter (`L.elem` ['a' .. 'z'])) lines
  return (L.nub words)