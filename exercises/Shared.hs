{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Shared where

import GHC.TypeLits (Symbol)
import Squeal.PostgreSQL

type family Columns (x :: Symbol) :: ColumnsType
type family Constraints (x :: Symbol) :: TableConstraints

type SimpleTable (x :: Symbol) = x ::: 'Table (Constraints x :=> Columns x)
