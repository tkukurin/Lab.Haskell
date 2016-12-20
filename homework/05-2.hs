data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

right (Node x l r) = r
left (Node x l r)  = l
val (Node x l r)   = x

addRight :: Tree a -> Tree a -> Tree a
addRight Leaf addNode = addNode 
addRight (Node val left right) addNode = Node val left (addRight right addNode)

treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter f Leaf           = Leaf
treeFilter f (Node val l r) = if f val 
  then Node val (treeFilter f l) (treeFilter f r) 
  else Leaf

levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f node = levelMapWithDepth 0 f node
  where levelMapWithDepth _ _ Leaf = Leaf
        levelMapWithDepth depth f (Node val l r) = 
          Node (f depth val) (nextDepth l) (nextDepth r)
          where nextDepth = levelMapWithDepth (depth + 1) f

isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf Leaf = True
isSubtree _ Leaf = False
isSubtree Leaf _ = False
isSubtree (Node v2 l2 r2) (Node v1 l1 r1) = 
  currentMatch || (isSubtree matchNode l1) || (isSubtree matchNode r1) 
  where currentMatch = if v1 == v2 then (isSubtree l1 l2) && (isSubtree r1 r2) else False
        matchNode = Node v2 l2 r2

