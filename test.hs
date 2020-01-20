-- In order for a RegTree to be valid, it must have a different int for each node.
data RegTree a = Node Tree Tree | Leaf a | Recurse Int -- the int is number of layers from top
-- e.g. T = T -> int would be
-- Node (Recurse 0) (Leaf int)



left :: RegTree a -> RegTree a
left Node t1 t2 =
