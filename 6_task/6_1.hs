import Prelude hiding ((!!))

data DList a = DEmpty | DCons (DList a) a (DList a)

instance (Eq a) => Eq (DList a) where
	DEmpty == DEmpty = True
	(DCons _ lh lt) == (DCons _ rh rt) = (lh == rh) && (lt == rt)

instance (Show a) => Show (DList a) where
	show lst = "[" ++ show' lst ++ "]" where
		show' DEmpty = ""
		show' (DCons _ h DEmpty) = show h
		show' (DCons _ h t) = show h ++ ", " ++ show' t


list2dlist' _ [] = DEmpty
list2dlist' left (h:t) =
	let rec = DCons left h (list2dlist' rec t) in
	rec
list2dlist lst = list2dlist' DEmpty lst

insert :: DList a -> Integer -> a -> DList a
insert _ i _  | i < 0 = error "Index can't be negative"
insert DEmpty _ el  = DCons DEmpty el DEmpty
insert (DCons l c r) 0 el = newNode where
	newNode = DCons l el (DCons newNode c r)
insert (DCons l c DEmpty) i el = updNode where
	updNode = DCons l c (DCons updNode el DEmpty)
insert (DCons l c r) i el = DCons l c (insert r (i-1) el) 	

remove :: DList a -> Integer -> DList a
remove _ i | i < 0 = error "Index can't be negative"
remove DEmpty _ = DEmpty
remove (DCons DEmpty c (DCons l c' r)) 0 = DCons DEmpty c' r
remove (DCons _ c DEmpty) i	| i == 0 		= DEmpty
							| otherwise 	= error "Index is out of bounds of the list"
remove (DCons (DCons ll lc lr) c (DCons rl rc rr)) 0 = updNode where
	updNode = DCons (DCons ll lc updNode) rc rr
remove (DCons l c r) i = DCons l c (remove r (i - 1))
									
(!!) :: DList a -> Integer -> a
(!!) lst i | i < 0 = error "Index can't be negative"
		   | otherwise = case lst of 
		   		(DCons l c r) -> if i == 0 then c else r !! (i - 1)
		   		DEmpty -> error "Index is out of bounds of the list"