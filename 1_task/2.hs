-- ������� �� ������ ���� �������

myCos :: Double -> Int -> Double

myCos x 0 = 1

myCos x n = myCos x (n-1) + (-1)^n * x^(2*n) / (fromIntegral $ product [1..2*n])



-- ���������� ����� �������� ���� �����

-- http://younglinux.info/algorithm/euclidean

(?) :: Int -> Int -> Int

a ? b

  | a < 0 || b < 0 = error "�� �������"

  | a == b = a

  | a > b = (a - b) ? b

  | a < b = a ? (b - a)



-- ���������� ������ ����� � ����� �������

-- https://www.rookieslab.com/posts/fast-power-algorithm-exponentiation-by-squaring-cpp-python-implementation

(^*^) :: Int -> Int -> Int

_ ^*^ 0 = 1

0 ^*^ _ = 0

a ^*^ b

  | even b = (a*a) ^*^ (b `div` 2)

  | odd b = a * (a ^*^ (b-1))