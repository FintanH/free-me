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
    Tell (String -> a)
  | Say String a
  deriving (Functor)

type MiMo a = Free MiMoF a

tell :: MiMo String
tell = liftF $ Tell id

say :: String -> MiMo ()
say str = liftF $ Say str ()

echo :: MiMo ()
echo = do
  str <- tell
  say str

toIO :: MiMo a -> IO a
toIO = foldFree go
  where
    go (Tell f) = f <$> getLine
    go (Say str next) = putStrLn str >> pure next
