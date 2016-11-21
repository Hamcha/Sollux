{-# LANGUAGE Safe #-}

{-|
Module      : Utils
Description : Util functions for other modules
-}
module Utils
( abspath
, fallback
, must
, prependM
, prependMB
) where

import safe qualified System.FilePath as Filepath (combine)

-- | Lift value from Maybes
must :: Maybe a -> a
must (Just c) = c
must _        = error "Could not find required value!"

-- | Lift Maybe with fallback
fallback :: a -> Maybe a -> a
fallback _ (Just c) = c
fallback f Nothing  = f

-- | Return the path if absolute, prepend a base path if relative
abspath :: FilePath -> FilePath -> FilePath
abspath _    ('/':path) = '/' : path
abspath base x          = Filepath.combine base x

-- | Prepend a value to a monadic list
prependM :: Monad m => a -> m [a] -> m [a]
prependM a b = b >>= \lst -> return $ a:lst

-- | Prepend a monadic value to a monadic list
prependMB :: Monad m => m a -> m [a] -> m [a]
prependMB a b = a >>= \val -> prependM val b