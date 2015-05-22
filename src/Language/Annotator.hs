{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Language.Annotator
-- Description : Annotator for Syntax Tree
-- Copyright   : (c) Nobuo Yamashita, 2015
-- License     : BSD3
-- Maintainer  : nobsun@sampou.org
-- Stability   : experimental
module Language.Annotator (
    -- * Classes
     Annotator (..)
    ,Annotatable (..)
    -- * Newtype
    ,I
    ) where

import Control.Comonad (Comonad (extract))

-- |
-- Equivalent for Control.Monad.Identity.Identity
newtype I a
  = I { unI :: a -- ^ unwraping 
      }

-- |
-- Annotator class
class Comonad fa => Annotator fa where
  -- | getting annotating type
  type Annotation fa 
  -- | getting annotating
  annotating :: forall b . fa b -> Annotation fa
  -- | getting annotated
  annotated :: forall b . fa b -> b
  annotated =  extract

  annotateWith :: forall b . Annotation fa -> b -> fa b
  
instance Annotator ((,) a) where
  type Annotation ((,) a) = a
  annotating = fst

-- |
-- Annotatable class
class Annotator f => Annotatable t f where
  annotate   :: t I a -> f (t f a)
  unannotate :: f (t f a) -> t I a 

