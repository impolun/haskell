data BinaryTree = EmptyTree
                  | Node {value::Integer, left::BinaryTree, right::BinaryTree}


insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree new_val = Node new_val EmptyTree EmptyTree
insert (Node v l r) c =
  if c < v
    then Node v (insert l c) r
  else
    Node v l (insert r c)


emptyTree :: BinaryTree
emptyTree = EmptyTree


containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Node v l r) c
  | v == c = True
  | v < c = containsElement l v
  | v > c = containsElement r v

nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree _ = error "Подходящее значение не найдено"
nearestGE (Node v l r) c
  | v == c = v 
  | v > c = if value l > c then nearestGE l c else v
  | v < c = nearestGE r c


leftMostElement :: BinaryTree -> BinaryTree
leftMostElement EmptyTree = EmptyTree
leftMostElement node@(Node _ EmptyTree _) = node
leftMostElement (Node _ l _ ) = leftMostElement l



remove :: BinaryTree -> Integer -> BinaryTree

remove EmptyTree _ = EmptyTree

remove (Node x l r) k
  
  | k > x = Node x l (remove r k)
  
  | k < x = Node x (remove l k) r
 
  | k == x = remove' (Node x l r)
  where
    remove' :: BinaryTree -> BinaryTree
    
    remove' (Node _ EmptyTree EmptyTree) = EmptyTree
    remove' (Node _ l EmptyTree) = l
    remove' (Node _ EmptyTree r) = r
    remove' (Node _ l r)
      | EmptyTree <- (left r) = Node (value r) l (right r)
      | otherwise = Node (value lmax) l (remove r $ value lmax)
      where lmax = leftMostElement r
treeFromList :: [Integer] -> BinaryTree
treeFromList lst = foldl insert EmptyTree lst
listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = []
listFromTree tree = let
  leftmost_value = value $ leftMostElement tree
  tree_wo_leftmost_value = remove tree leftmost_value in
  leftmost_value : listFromTree tree_wo_leftmost_value