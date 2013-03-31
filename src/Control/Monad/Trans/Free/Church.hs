{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Trans.Free.Church
    (FT(..)
    , MonadFree(..)
    , hoistFT
    , transFT
    , cataFT
    , foldFT
    , sequenceFT
    ) where

import Prelude hiding (mapM, sequence)
import Control.Applicative
import Control.Monad (join, (<=<))
import Control.Monad.Free.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Data.Traversable

newtype FT f m a = FT { runFT :: forall r. (a -> m r) -> (f (m r) -> m r) -> m r }

instance Functor (FT f m) where
  fmap f (FT g) = FT $ \kp -> g (kp . f)

instance Applicative (FT f m) where
  pure a = FT $ \kp _ -> kp a
  FT f <*> FT g = FT $ \kp kf -> f (\a -> g (\b -> kp $ a b) kf) kf
  
instance Monad (FT f m) where
  return a = FT $ \kp _ -> kp a
  FT m >>= f = FT $ \kp kf -> m (\a -> runFT (f a) kp kf) kf

instance (Functor f) => MonadFree f (FT f m) where
  wrap a = FT $ \kp kf -> kf $ fmap (\f -> runFT f kp kf) a
  
instance MonadTrans (FT f) where
  lift a = FT $ \kp _ -> a >>= kp

hoistFT :: (Functor f, Monad m, Monad n) => (forall a. m a -> n a) -> FT f m b -> FT f n b
hoistFT mu (FT f) = FT $ \kp kf -> join . mu . f (return . kp) $ return . kf . fmap (join . mu)

transFT :: (forall a. f a -> g a) -> FT f m b -> FT g m b
transFT mu (FT f) = FT $ \kp kf -> f kp (kf . mu)

cataFT :: (a -> m b) -> (f (m b) -> m b) -> FT f m a -> m b
cataFT kp kf ft = runFT ft kp kf

foldFT :: (Traversable f, Monad m) => (a -> m b) -> (f b -> m b) -> FT f m a -> m b
foldFT kp kf = cataFT kp $ kf <=< sequence

sequenceFT :: (Traversable f, Monad m, MonadFree f n) => FT f m a -> m (n a)
sequenceFT = foldFT (return . return) (return . wrap)

toFreeT :: (Functor f, Monad m) => FT f m a -> FreeT f m a
toFreeT = FreeT . cataFT (runFreeT . return) (runFreeT . wrap . fmap FreeT)

fromFreeT :: (Functor f, Monad m) => FreeT f m a -> FT f m a
fromFreeT f = FT $ \kp kf -> cataFreeT kp kf f 