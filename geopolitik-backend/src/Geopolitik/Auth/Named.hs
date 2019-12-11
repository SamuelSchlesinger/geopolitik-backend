module Geopolitik.Auth.Named 
  ( named
  , Named 
  , anon ) where

newtype Named name a = Named a

named :: a -> (forall name. Named name a -> b) -> b
named a f = f (Named a)

anon :: Named name a -> a
anon (Named a) = a
