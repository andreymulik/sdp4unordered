{-# LANGUAGE Safe, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.HashSet
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
    @SDP.HashSet@ provides 'HashSet' - unordered set with 'Hashable' keys.
-}
module SDP.HashSet
(
  -- * Exports
  module SDP.Hashable,
  module SDP.Linear,
  module SDP.Set,
  
  -- * Hash set
  HashSet
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Hashable
import SDP.Linear
import SDP.Set

import qualified Data.HashSet as H
import Data.HashSet ( HashSet )

default ()

--------------------------------------------------------------------------------

{- Nullable, Estimate and Bordered instances. -}

instance Nullable (HashSet e)
  where
    isNull = null
    lzero  = H.empty

instance (Eq e, Hashable e) => Estimate (HashSet e)
  where
    (<==>) = on (<=>) sizeOf
    (.<=.) = on (<=)  sizeOf
    (.>=.) = on (>=)  sizeOf
    (.>.)  = on (>)   sizeOf
    (.<.)  = on (<)   sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf

instance (Eq e, Hashable e) => Bordered (HashSet e) Int
  where
    lower    _ = 0
    sizeOf     = length
    upper   xs = length xs - 1
    rebound bs = H.fromList . rebound (H.toList bs)

--------------------------------------------------------------------------------

instance (Eq e, Hashable e) => Set (HashSet e) e
  where
    set    = id -- always correct
    member = H.member
    insert = H.insert
    delete = H.delete
    
    unions        = foldr1 (\/)
    symdiffs      = foldr1 (\^/)
    differences   = foldr1 (/\)
    intersections = foldr1 (\\)
    
    (/\) = H.intersection
    (\\) = H.difference
    (\/) = H.union
    
    xs \^/ ys = (xs \/ ys) \\ (xs /\ ys)
    xs \+/ ys = isNull (ys \\ xs)
    xs /?\ ys = isNull (xs /\ ys)
    xs \?/ ys = not (xs /?\ ys)
    
    lookupLT e = lookupLT e . H.toList
    lookupGT e = lookupGT e . H.toList
    lookupLE e = lookupLE e . H.toList
    lookupGE e = lookupGE e . H.toList




