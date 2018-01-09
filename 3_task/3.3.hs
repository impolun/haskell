newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }
newtype PSet3 a = PSet3{ contains3 :: (a -> Bool) }

-- �������� ��������
instance Monoid (PSet a) where
  mempty = PSet (\a -> False)
  mappend (PSet f1) (PSet f2) = PSet (\a -> (f1 a) || (f2 a))

-- ����������� ��������
instance Monoid (PSet2 a) where
  mempty = PSet2 (\a -> True)
  mappend (PSet2 f1) (PSet2 f2) = PSet2 (\a -> (f1 a) && (f2 a))

-- �������������� ��������
instance Monoid (PSet3 a) where
  mempty = PSet3 (\a -> False)
  mappend (PSet3 f1) (PSet3 f2) = PSet3 (\a -> ((f1 a) && (not $ f2 a)) || ((not $ f1 a) && (f2 a)))

-- �������
-- ��������� ������ ���������� False, ������ ��� ���� ������ ����������� �� ��������� A � B 
-- �� �� ����� ������� ������ � ��������� B 
instance Functor PSet where
  fmap f (PSet fa) = PSet (\b -> False)

-- test functions
test :: String -> String
test str = "Nothing"
main = interact test