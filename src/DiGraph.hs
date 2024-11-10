module DiGraph where

type DiGraph a = [(a, [a])]

member :: Eq a => a -> [(a, b)] -> Bool
member key graph =
    case lookup key graph of
        Just _ -> True
        Nothing -> False

hasNode :: Eq a => DiGraph a -> a -> Bool
hasNode = flip member

addNode :: Eq a => DiGraph a -> a -> DiGraph a
addNode graph node
  | graph `hasNode` node = graph
  | otherwise = (node, []) : graph