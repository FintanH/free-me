{-# LANGUAGE DeriveFunctor #-}

module MiMo
    ( MiMo
    , tell
    , say
    , echo
    , toIO
    ) where

import           Free

data MiMoF a =
  -- | Tell is waiting for a string to do something so therefore it's
  -- | a function of String the next action
    Tell (String -> a)
  -- | Say is outputting a String and then continues on with its next action
  | Say String a
  deriving (Functor)

type MiMo a = Free MiMoF a

-- | Interestingly, we have a kind of dual for Tell and Say in the signatures
-- | Where Tell contained a function, it does not take a parameter in `tell`
-- | On the other hand, `Say` was not a function, but `say` becomes a function
-- | from String to MiMo

tell :: MiMo String
tell = liftF $ Tell id

say :: String -> MiMo ()
say str = liftF $ Say str ()

-- | `echo` shows off the power of Free by being able to combine actions in do
-- | syntax and we haven't even said what tell and say really do
echo :: MiMo ()
echo = do
  str <- tell
  say str

-- | This is our interpreter from MiMo to IO. We create a natural transformation,
-- | From MiMoF to IO and then utilise `foldFree` to do all the heavy lifting
toIO :: MiMo a -> IO a
toIO = foldFree go
  where
    go (Tell f) = f <$> getLine
    go (Say str next) = putStrLn str >> pure next
