import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

data FunMonad a = FunMonad{ fun  () - a }

instance (Show fun) = Show (FunMonad fun) where
  show (FunMonad fun) = show FunMonad  ++ (show $ fun ())

-- httpswiki.haskell.orgFunctor-Applicative-Monad_Proposal#Missing_superclasses
instance Functor FunMonad where
    fmap = liftM

instance Applicative FunMonad where
    pure x = FunMonad (() - x)
    () = ap

instance Monad FunMonad where
    return = pure
    m = k = k $ fun m ()

test = do
  fm - FunMonad (() - 5)
  return fm = (v - FunMonad $ () - [v, v, v])