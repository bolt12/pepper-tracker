{-# LANGUAGE DuplicateRecordFields #-}

module Models.Entity where

-- (1) Type Classes -----

class Entity a where
    getId :: a -> Int
