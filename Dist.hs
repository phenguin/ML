module Dist where
import Control.Monad
import System.Environment
import Control.Applicative
import Control.Monad.Trans

newtype Prob = P {flt :: Float} deriving (Eq, Ord)

instance Show Prob where
	show (P x) = show whole ++ "." ++ show frac ++ "%"
		where digits = round $ 1000 * x
		      whole = digits `div` 10
		      frac = digits `mod` 10

instance Num Prob where
	(P 0) * (P _) = P 0
	P x * P y = P (x*y)
	P x + P y = P (x+y)
	fromInteger n = P (fromInteger n)
	abs (P x) = P (abs x)

data Perhaps a = Perhaps a Prob deriving (Show)

instance Monad Perhaps where
	return x = Perhaps x (P 1)
	(Perhaps x (P p)) >>= f =
		Perhaps y (P $ p*p')
			where (Perhaps y (P p')) = f x

newtype PerhapsT m a = PerhapsT {runPerhapsT :: m (Perhaps a)}

instance (Monad m) => Monad (PerhapsT m) where
	return x = PerhapsT $ return (Perhaps x (P 1))
	tma >>= f = PerhapsT $ do (Perhaps x p) <- runPerhapsT tma 
				  (Perhaps y p') <- runPerhapsT (f x)
				  return $ Perhaps y (p*p')

instance MonadTrans PerhapsT where
	lift x = PerhapsT $ liftM (\y->Perhaps y (P 1.0)) x

----------------------------------------------------------------------
----------------------------------------------------------------------
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}
instance (Monad m) => Monad (MaybeT m) where
	return = MaybeT . return . Just
	tma >>= f = MaybeT $ do maybe_val <- runMaybeT tma
				case maybe_val of
				     Just val -> runMaybeT (f val)
				     Nothing -> return $ Nothing

instance MonadTrans MaybeT where
	lift x = MaybeT $ liftM Just x
	
----------------------------------------------------------------------
----------------------------------------------------------------------
type Dist = PerhapsT [] 
type BDist = MaybeT Dist
		       
weighted :: [(Float,a)] -> PerhapsT [] a
weighted xs = PerhapsT $ map (\(p,x) -> Perhaps x (P (p / total))) xs
	where total = sum $ map fst xs

weighted' :: [(Float,a)] -> BDist a
weighted' = lift . weighted
uniform :: [a] -> PerhapsT [] a
uniform xs = weighted (map (\x->(1,x)) xs)
