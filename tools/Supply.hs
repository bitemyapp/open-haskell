{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             GeneralizedNewtypeDeriving,
             PackageImports
  #-}

-- Supply monad, taken from
-- http://www.haskell.org/haskellwiki/New_monads/MonadSupply

module Supply
    (SupplyT,
     MonadSupply,
     supply,
     Supply,
     evalSupplyT,
     evalSupply,
     runSupplyT,
     runSupply)
    where
import Control.Monad
import "mtl" Control.Monad.State

import "mtl" Control.Monad.Identity

newtype SupplyT s m a = SupplyT (StateT [s] m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)

newtype Supply s a = Supply (SupplyT s Identity a)
    deriving (Functor, Monad, MonadSupply s)

class Monad m => MonadSupply s m | m -> s where
    supply :: m s

instance Monad m => MonadSupply s (SupplyT s m) where
    supply = SupplyT $ do
                (x:xs) <- get
                put xs
                return x

evalSupplyT :: (Monad m) => SupplyT s m a -> [s] -> m a
evalSupplyT (SupplyT s) supp = evalStateT s supp

evalSupply :: Supply s a -> [s] -> a
evalSupply (Supply s) supp = runIdentity $ evalSupplyT s supp

runSupplyT (SupplyT s) supp = runStateT s supp
runSupply (Supply s) supp = runSupplyT s supp