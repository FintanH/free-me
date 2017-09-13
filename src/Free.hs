{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types   #-}

module Free where

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap Pure

foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = pure a

-- | freer :: f (Free f a)
-- | k freer :: m (Free f a)
-- | (foldFree f freer >>=) :: (Free f a -> m (Free f b))
-- | Substitute m a for m (Free f b)
-- | Free f a -> m a
-- | foldFree k :: Free f a -> m a
foldFree k (Free freer) = k freer >>= foldFree k

data Free f a =
    Free (f (Free f a))
  | Pure a

instance Functor f => Functor (Free f) where
  -- | This case we just unwrap the `a`
  -- | Apply our function `f` and re-wrap in Pure
  fmap f (Pure a) = Pure $ f a

  -- | Here again, we unwrap and wrap with `Free`
  -- | We have the inner structure `freer` which looks like
  -- | f (Free f a), and we know `f` as `Functor`
  -- | So we can `fmap` over the `Functor` and now we're inside to get
  -- | Free f a.
  -- | The trick then is to `fmap` again as a recursive case.
  fmap f (Free freer) = Free $ fmap (fmap f) freer

instance Functor f => Applicative (Free f) where
  -- | Pretty much says it in the name
  pure = Pure

  -- | Base cases of <*>
  (Pure f) <*> (Pure a) = Pure $ f a
  (Pure f) <*> (Free freer) = Free $ fmap (fmap f) freer

  -- | So, freeFunc :: f (Free f (a -> b)) <- this is cool because what does
  -- | <*> usually look like? That's right `f (a -> b) -> f a -> f b`
  -- | and freer :: f (Free f a)
  -- | and I need a Free f b
  -- | I think we need to get out the `Free f (a -> b)` so that we can recursively
  -- | call <*> on `freer`
  -- | So what we really want to end up with is two variables:
  -- | innerFunc :: Free f (a -> b) and
  -- | innerFreer :: Free f a so we can do
  -- | innerFunc <*> innerFreer
  -- | those two inner things are the inside the functor of freeFunc and freer.

  -- | So I cheated and looked up the answer and I was overthinking it.
  -- | We just need to unwrap the Free wrapped function
  -- | So then we have f (Free f (a -> b)) and the on the other side we have
  -- | freer :: Free f a
  -- | This leaves us by simply trying to access the inner function structure of
  -- | freeFunc and applying it to freeFunc in the recursive case.
  -- | so simply fmap (<*> freer) freeFunc
  -- | then wrap it all back up in Free!
  (Free freeFunc) <*> freer =
    Free $ fmap (<*> freer) freeFunc

instance Functor f => Monad (Free f) where
  return = pure
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  -- | Easy money
  (Pure a) >>= k = k a

  -- | Again, we have freer :: f (Free f a)
  -- | and a function k :: (a -> Free f b)
  -- | We need to get in at the `a` of freer
  -- | We can remove one level by fmap over freer
  -- | This leaves us with Free f a
  -- | and of course (>>=) :: Free f a -> (a -> Free f b) -> Free f b so
  -- | we just fmap (>>= k) freer and wrap it all back up!
  (Free freer) >>= k = Free $ fmap (>>= k) freer
