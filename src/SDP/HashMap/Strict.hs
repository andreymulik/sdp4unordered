{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.HashMap.Strict
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
    @SDP.HashMap.Strict@ provides 'HashMap' - strict unordered associative array
    with 'Hashable' keys.
-}
module SDP.HashMap.Strict
(
  -- * Exports
  module SDP.Hashable,
  module SDP.Linear,
  module SDP.Map,
  
  -- * Hash map
  HashMap, SHashMap
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Hashable
import SDP.Linear
import SDP.Map

import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict ( HashMap )

import Data.Function
import Data.Maybe

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'HashMap' alias, may reduce ambiguity.
type SHashMap = HashMap

--------------------------------------------------------------------------------

{- Nullable, Estimate and Bordered instances. -}

instance Nullable (HashMap k e)
  where
    isNull = null
    lzero  = H.empty

instance (Index k) => Estimate (HashMap k e)
  where
    (<==>) = on (<=>) length
    (.>.)  = on  (>)  length
    (.<.)  = on  (<)  length
    (.<=.) = on  (<=) length
    (.>=.) = on  (>=) length
    
    (<.=>) = (<=>) . length
    (.>)   = (>)   . length
    (.<)   = (<)   . length
    (.>=)  = (>=)  . length
    (.<=)  = (<=)  . length

--------------------------------------------------------------------------------

{- Map instance. -}

instance (Eq k, Hashable k) => Map (HashMap k e) k e
  where
    toMap' = const toMap
    toMap  = H.fromList
    assocs = H.toList
    
    filter' = H.filterWithKey
    member' = H.member
    insert' = H.insert
    delete' = H.delete
    
    -- | Throws 'IndexException' instead 'error' call.
    (!)  = fromMaybe (undEx "(!) {HashMap k e}") ... (!?)
    (!?) = flip H.lookup
    
    (//) = toMap ... (++) . assocs
    keys = H.keys
    
    kfoldl = H.foldlWithKey' . flip
    kfoldr = H.foldrWithKey

--------------------------------------------------------------------------------

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.HashMap.Strict."



