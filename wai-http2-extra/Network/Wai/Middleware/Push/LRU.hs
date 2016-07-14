{-# LANGUAGE BangPatterns #-}
-- From https://jaspervdj.be/posts/2015-02-24-lru-cache.html
module Network.Wai.Middleware.Push.LRU (
    LRUCache(..)
  , empty
  , null
  , alter
  , lookup
  ) where

import           Control.Applicative ((<$>))
import qualified Data.OrdPSQ         as PSQ
import           Data.Int            (Int64)
import           Prelude             hiding (lookup, null)

type Priority = Int64

data LRUCache k v = LRUCache {
      cCapacity :: !Int       -- ^ The maximum number of elements in the queue
    , cSize     :: !Int       -- ^ The current number of elements in the queue
    , cTick     :: !Priority  -- ^ The next logical time
    , cQueue    :: !(PSQ.OrdPSQ k Priority v)
    } deriving (Eq, Show)

empty :: Int -> LRUCache k v
empty capacity
    | capacity < 1 = error "LRUCache.empty: capacity < 1"
    | otherwise    = LRUCache {
          cCapacity = capacity
        , cSize     = 0
        , cTick     = 0
        , cQueue    = PSQ.empty
        }

null :: LRUCache k v -> Bool
null c = cSize c == 0

trim :: Ord k => LRUCache k v -> LRUCache k v
trim c
    | cTick c == maxBound   = empty (cCapacity c)
    | cSize c > cCapacity c = c {
          cSize  = cSize c - 1
        , cQueue = PSQ.deleteMin (cQueue c)
        }
    | otherwise             = c

alter :: Ord k => (Maybe v -> (Bool, Maybe v))
               -> k
               -> LRUCache k v
               -> LRUCache k v
alter func key c = trim $! cache
  where
    alt Nothing      = case func Nothing of
      (exist, Nothing) -> (exist, Nothing)
      (exist, Just v') -> (exist, Just (cTick c, v'))
    alt (Just (_,v)) = case func (Just v) of
      (exist, Nothing) -> (exist, Nothing)
      (exist, Just v') -> (exist, Just (cTick c, v'))
    (exst, queue) = PSQ.alter alt key (cQueue c)
    cache = c {
          cSize  = if exst then cSize c else cSize c + 1
        , cTick  = cTick c + 1
        , cQueue = queue
        }

lookup :: Ord k => k -> LRUCache k v -> Maybe (v, LRUCache k v)
lookup k c = case PSQ.alter lookupAndBump k (cQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q)  -> let !c' = trim $ c {cTick = cTick c + 1, cQueue = q}
                    in Just (x, c')
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x,  Just ((cTick c), x))
