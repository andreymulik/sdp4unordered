{-# LANGUAGE Safe, CPP, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.HashMap.Lazy
    Copyright   :  (c) Andrey Mulik 2020-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
    @SDP.HashMap.Lazy@ provides 'HashMap' - lazy unordered associative array
    with 'Hashable' keys.
-}
module SDP.HashMap.Lazy
(
  -- * Exports
  module SDP.Hashable,
  module SDP.Linear,
  module SDP.Map,
  
  -- * Hash map
  HashMap, LHashMap
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Hashable
import SDP.Linear
import SDP.Map

import qualified Data.HashMap.Lazy as H
import Data.HashMap.Lazy ( HashMap )

import Data.Maybe

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'HashMap' alias, may reduce ambiguity.
type LHashMap = HashMap

--------------------------------------------------------------------------------

#if MIN_VERSION_sdp(0,3,0)
instance Forceable (HashMap k e)
#endif

instance Nullable (HashMap k e)
  where
    isNull = null
    lzero  = H.empty

instance Index k => Estimate (HashMap k e)
  where
    (<==>) = on (<=>) length
    (.<=.) = on (<=)  length
    (.>=.) = on (>=)  length
    (.>.)  = on (>)   length
    (.<.)  = on (<)   length
    
    (<.=>) = (<=>) . length
    (.>=)  = (>=)  . length
    (.<=)  = (<=)  . length
    (.>)   = (>)   . length
    (.<)   = (<)   . length
    
#if MIN_VERSION_sdp(0,3,0)
    sizeOf = length
#endif

instance (Eq k, Hashable k) => Map (HashMap k e) k e
  where
#if MIN_VERSION_sdp(0,3,0)
    update' es f key = H.update (Just . f) key es
    write'  es key e = H.insert key e es
#endif
    
    kfoldl = H.foldlWithKey' . flip
    kfoldr = H.foldrWithKey
    toMap' = const toMap
    toMap  = H.fromList
    assocs = H.toList
    
    filter' = H.filterWithKey
    member' = H.member
    insert' = H.insert
    delete' = H.delete
    
    -- | Throws 'IndexException' instead 'error' call.
    (!)  = fromMaybe (undEx "(!) {HashMap k e}") ... (!?)
    (//) = toMap ... (++) . assocs
    (!?) = flip H.lookup
    keys = H.keys

--------------------------------------------------------------------------------

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.HashMap.Lazy."




