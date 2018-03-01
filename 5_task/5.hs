{-# LANGUAGE RankNTypes #-}

import Control.Lens
import ReverseList

errorEmptyList = error "Cannot focus on an empty ReverseList"

-- �����, �������������� �� ������ ������������� ������ (��� R[1,2,3] - ��� 3)
rheadLens :: Lens' (ReverseList a) a
rheadLens = lens getter setter where
    getter RNil = errorEmptyList
    getter (RCons _ elem) = elem
    setter RNil _ = errorEmptyList
    setter (RCons rlst elem) newelem = RCons rlst newelem

-- ������� ��� rheadLens
rhead :: ReverseList a -> a
rhead = view rheadLens

exampleRList = fromList [1..5]
rheadLens_Example1 = rhead exampleRList -- ������� ������ ������ R[1..5], �� ���� ����� 5
rheadLens_Example2 = over rheadLens (*2) exampleRList -- ������� ������ � ��������� �������, �.�. ������ 10 ������ 5
rheadLens_Example3 = fromList [(x,y) | x <- [1..3], y <- ['a'..'z']] ^. rheadLens . _2 -- ���������� ���� (������� 'z')

-- �����, �������������� �� ������ ������������� ������ (��� R[1,2,3] - ��� R[1,2])
rtailLens :: Lens' (ReverseList a) (ReverseList a)
rtailLens = lens getter setter where
    getter RNil = errorEmptyList
    getter (RCons rlst _) = rlst
    setter RNil _ = errorEmptyList
    setter (RCons rlst elem) newrlst = RCons newrlst elem

-- ������� ��� rtailLens
rtail :: ReverseList a -> ReverseList a
rtail = view rtailLens

rtailLens_Example1 = rtail exampleRList -- ������� ����� ������ R[1..5], �� ���� R[1,2,3,4]
rtailLens_Example2 = set rtailLens (fromList [42,43]) exampleRList -- ������� ������ R[42,43,5]

-- �����, �������������� �� last ������������� ������ (��� R[1,2,3] - ��� 1)
rlastLens :: Lens' (ReverseList a) a
rlastLens = lens getter setter where
    getter RNil = errorEmptyList
    getter (RCons RNil elem) = elem
    getter (RCons rlst _) = getter rlst
    setter RNil _ = errorEmptyList
    setter (RCons RNil elem) newelem = RCons RNil newelem
    setter (RCons rlst elem) newelem = RCons (setter rlst newelem) elem

-- ������� ��� rlastLens
rlast :: ReverseList a -> a
rlast = view rlastLens

rlastLens_Example1 = rlast exampleRList -- ������� last ������ R[1..5], �� ���� ����� 1
rlastLens_Example2 = fmap show exampleRList & rlastLens %~ (++ " - this is the last") -- ������ �� last

-- �����, �������������� �� init ������������� ������ (��� R[1,2,3] - ��� R[2,3])
rinitLens :: Lens' (ReverseList a) (ReverseList a)
rinitLens = lens getter setter where
    getter RNil = errorEmptyList
    getter (RCons RNil _) = RNil
    getter (RCons rlst elem) = RCons (getter rlst) elem
    setter RNil _ = errorEmptyList
    setter (RCons RNil _) newrlst = RCons (rtail newrlst) (rhead newrlst)
    setter (RCons rlst _) newrlst = RCons (RCons RNil (rlast rlst) `rconcat` rtail newrlst) (rhead newrlst)

-- ������� ��� rinitLens
rinit :: ReverseList a -> ReverseList a
rinit = view rinitLens

rinitLens_Example1 = rinit exampleRList -- ������� init ������ R[1..5], �� ���� R[2,3,4,5]
rinitLens_Example2 = exampleRList & rinitLens .~ rtail exampleRList -- ������� init �� tail

-- �����, �������������� �� �������� ������������� ������ �� ��� ������� (��� R[1,2,3] ������� � �������� 0 - ��� 3)
rindexLens :: Int -> Lens' (ReverseList a) a
rindexLens index = if index < 0 then error "Negative index" else lens getter setter where
    getter RNil = errorEmptyList
    getter rlist = rget rlist index where 
        rget :: ReverseList a -> Int -> a
        rget (RCons _ elem) 0 = elem
        rget (RCons RNil _) _ = error "Index out of bounds"
        rget (RCons rlst _) idx = rget rlst $ pred idx
    setter RNil _ = errorEmptyList
    setter rlist newelem = rset rlist newelem index where
        rset :: ReverseList a -> a -> Int -> ReverseList a
        rset (RCons rlst _) new 0 = RCons rlst new
        rset (RCons RNil _) _ _ = error "Index out of bounds"
        rset (RCons rlst elem) new idx = rset rlst new (pred idx) `rconcat` RCons RNil elem

-- ������� ��� rindexLens
rindex :: ReverseList a -> Int -> a
rindex rlist idx = rlist ^. rindexLens idx

rindexLens_Example1 = exampleRList `rindex` 1 -- ������ ����� 4 �� ������ R[1..5]
rindexLens_Example2 = set (rindexLens 1) 100500 exampleRList -- ������ ������ R[1,2,3,100500,5]
rindexLens_Example3 = exampleRList & rindexLens 4 %~ negate -- ������ ������ R[-1,2,3,4,5]