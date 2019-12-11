{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Geopolitik.Ontology 
  ( Article(..)
  , Collaborator(..)
  , Comment(..)
  , Coordinate(..)
  , Draft(..)
  , ExecutedMigration(..)
  , Key(..)
  , Link(..)
  , Location(..)
  , Session(..)
  , SomeTag(..)
  , Tag(..)
  , User(..)
  , allTags 
  , obscure
  , obvious
  ) where

import Geopolitik.Ontology.Key
import Geopolitik.Ontology.User
import Geopolitik.Ontology.Article
import Geopolitik.Ontology.Draft
import Geopolitik.Ontology.Session
import Geopolitik.Ontology.ExecutedMigration
import Geopolitik.Ontology.Location
import Geopolitik.Ontology.Comment
import Geopolitik.Ontology.Collaborator
import Geopolitik.Ontology.Tag
import Geopolitik.Ontology.Link

