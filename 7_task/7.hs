data Deque a = Deque [a] [a]

instance (Show a) => Show (Deque a) where
    show (Deque [] []) = "Deque[][]"
    show (Deque i o) = "Deque" ++ show i ++ show (reverse o)

emptyDeque :: Deque a 
emptyDeque = Deque [] []

enqueueLeft :: Deque a -> a -> Deque a 
enqueueLeft (Deque i o) x = Deque (x:i) o

enqueueRight :: Deque a -> a -> Deque a 
enqueueRight (Deque i o) x = Deque i (x:o)

dequeueLeft :: Deque a -> (Deque a, a) 
dequeueLeft (Deque [] []) = error "Deque is empty"
dequeueLeft (Deque [] [x]) = (emptyDeque, x)
dequeueLeft (Deque [] o) = let (lhalf, rhalf) = halves o in dequeueLeft $ Deque (reverse rhalf) lhalf
dequeueLeft (Deque (hi:ti) o) = (Deque ti o, hi)

dequeueRight :: Deque a -> (Deque a, a) 
dequeueRight (Deque [] []) = error "Deque is empty"
dequeueRight (Deque [x] []) = (emptyDeque, x)
dequeueRight (Deque i []) = let (lhalf, rhalf) = halves i in dequeueRight $ Deque lhalf (reverse rhalf)
dequeueRight (Deque i (ho:to)) = (Deque i to, ho)


halves :: [a] -> ([a], [a]) 
halves list = halve list list where
    halve :: [a] -> [a] -> ([a], [a])
    halve (x:tx) (_:_:tty) = let (dx, ddy) = halve tx tty in (x:dx, ddy)
    halve x _ = ([], x)