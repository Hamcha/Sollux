{-# LANGUAGE Safe #-}

{-|
Module      : Utils
Description : Util functions for other modules
-}
module Utils
( abspath
, must
) where

import safe qualified System.FilePath as Filepath (combine)

-- | Lift value from Maybes
must :: Maybe a -> a
must (Just c) = c
must _        = error "Could not find required value!"

-- | Return the path if absolute, prepend a base path if relative
abspath :: FilePath -> FilePath -> FilePath
abspath _    ('/':path) = '/' : path
abspath base x          = Filepath.combine base x