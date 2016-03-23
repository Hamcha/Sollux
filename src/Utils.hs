{-# LANGUAGE Safe #-}

{-|
Module      : Utils
Description : Util functions for other modules
-}
module Utils
( must
) where

-- | Lift value from Maybes
must :: Maybe a -> a
must (Just c) = c
must _        = error "Could not find required value!"