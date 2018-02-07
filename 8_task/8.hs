import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data Dyn = Fun ((Dyn) -> Dyn) | Str String | Number Integer | List [Dyn]

getType :: Dyn -> String
getType (Fun _) = "Function"
getType (Str _) = "String"
getType (Number _) = "Number"
getType (List _) = "List"

toInt :: Dyn -> Integer
toInt (Number v) = v
toInt (Str s) = fromMaybe err $ readMaybe s where
  err = error $ "String " ++ s ++ " does not contain numeric."
toInt t = error $ "Type cast from " ++ (getType t) ++ " to numeric is not supported."

instance Show Dyn where
  show (Str s) = s
  show (Number c) = show c
  show (List lst) = show lst
  show other = getType other

instance Num Dyn where
  l + r = Number $ (toInt l) + (toInt r)
  l * r = Number $ (toInt l) * (toInt r)
  abs d = Number $ abs $ toInt d
  signum d =  Number $ signum $ toInt d
  fromInteger = Number
  negate d = Number $ negate $ toInt d

instance Eq Dyn where
  a == b = toInt a == toInt b

instance Ord Dyn where
  a <= b = toInt a <= toInt b

instance Real Dyn where
  toRational = fromIntegral.toInt

instance Enum Dyn where
  toEnum = fromInteger.fromIntegral
  fromEnum = fromIntegral.toInt

instance Integral Dyn where
  toInteger = toInt
  quotRem a b = (Number $ fst res, Number $ snd res) where
    res = quotRem (toInt a) (toInt b)

getFun :: Dyn -> (Dyn -> Dyn)
getFun (Fun x) = x

-- �����������
-- https://en.wikipedia.org/wiki/SKI_combinator_calculus#Informal_description
_I :: Dyn -> Dyn
_I = id

_K :: Dyn
_K = Fun (\v -> Fun (\_ -> v))

_S :: Dyn -> Dyn -> Dyn -> Dyn
_S (Fun x) (Fun y) z = (\res -> getFun (x z) res) (y z)

testList = [Number 5, Str "testing", List [Str "TestInnerList", Str "TestInnerList2"], Fun id]

test_I = map _I testList
test_S = map (\v -> _S _K _K v) testList
