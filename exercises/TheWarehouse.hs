{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module TheWarehouse where

import Shared
import Squeal.PostgreSQL
import Data.Int (Int64)
import Data.Text (pack)

type instance Columns "warehouses" =
  '[ "code" ::: 'Def :=> 'NotNull 'PGint8
   , "location" ::: 'NoDef :=> 'NotNull 'PGtext
   , "capacity" ::: 'NoDef :=> 'NotNull 'PGint8 ]

type instance Constraints "warehouses" =
  '[ "pk_warehouses" ::: 'PrimaryKey '["code"] ]

type instance Columns "boxes" =
  '[ "code" ::: 'Def :=> 'NotNull 'PGtext
   , "contents" ::: 'NoDef :=> 'NotNull 'PGtext
   , "value" ::: 'NoDef :=> 'NotNull 'PGmoney
   , "warehouse" ::: 'NoDef :=> 'NotNull 'PGint8 ]

type instance Constraints "boxes" =
  '[ "pk_boxes" ::: 'PrimaryKey '["code"]
   , "fk_warehouse" ::: 'ForeignKey '["warehouse"] "warehouses" '["code"] ]

type DB = Public '[ SimpleTable "warehouses", SimpleTable "boxes" ]

-- | 1. Select all warehouses.
ex1 :: Query '[] '[] DB '[]
  '[ "code" ::: 'NotNull 'PGint8
   , "location" ::: 'NotNull 'PGtext
   , "capacity" ::: 'NotNull 'PGint8 ]
ex1 = select_ (#code :* #location :* #capacity) (from (table #warehouses))

-- | 2. Select all boxes with a value larger than $150.
ex2 :: Query '[] '[] DB '[]
  '[ "code" ::: 'NotNull 'PGtext
   , "contents" ::: 'NotNull 'PGtext
   , "value" ::: 'NotNull 'PGmoney
   , "warehouse" ::: 'NotNull 'PGint8 ]
ex2 = select_ (#code :* #contents :* #value :* #warehouse)
  (from (table #boxes)
  & where_ (#value .> inline (Money 15000)))

-- | 3. Select all distinct contents in all the boxes.
ex3 :: Query '[] '[] DB '[]
  '[ "code" ::: 'NotNull 'PGtext
   , "contents" ::: 'NotNull 'PGtext
   , "value" ::: 'NotNull 'PGmoney
   , "warehouse" ::: 'NotNull 'PGint8 ]
ex3 = selectDistinct_ (#code :* #contents :* #value :* #warehouse)
  (from (table #boxes)
  & where_ (#value .> inline (Money 15000)))

-- | 4. Select the average value of all the boxes.
ex4 :: Query '[] '[] DB '[]
  '[ "average_value" ::: 'NotNull 'PGmoney ]
ex4 = select_ (money `cast` avg (All $ numeric `cast` #value) `as` #average_value) (from (table #boxes) & groupBy #code)

-- | 5. Select the warehouse code and the average value of the boxes in each
-- warehouse.
ex5 :: Query '[] '[] DB '[]
  '[ "warehouse_code" ::: 'NotNull 'PGint8
   , "average_box_value" ::: 'Null 'PGmoney ]
ex5 = select_ (#w ! #code `as` #warehouse_code
            :* (money `cast` avg (All $ numeric `cast` (#b ! #value))) `as` #average_box_value)
  (from (table (#warehouses `as` #w) & innerJoin (table (#boxes `as` #b)) (#w ! #code .== #b ! #warehouse))
  & groupBy (#w ! #code))

-- | 6. Same as previous exercise, but select only those warehouses where the
-- average value of the boxes is greater than 150.
ex6 :: Query '[] '[] DB '[]
  '[ "warehouse_code" ::: 'NotNull 'PGint8
   , "average_box_value" ::: 'NotNull 'PGmoney ]
ex6 = select_ (#w ! #code `as` #warehouse_code
            :* (money `cast` avg (All $ numeric `cast` (#b ! #value))) `as` #average_box_value)
  (from (table (#warehouses `as` #w) & innerJoin (table (#boxes `as` #b)) (#w ! #code .== #b ! #warehouse))
  & groupBy (#w ! #code)
  & having ((money `cast` avg (All $ numeric `cast` (#b ! #value))) .> inline (Money 15000)))

-- | 7. Select the code of each box, along with the name of the city the box is located in.
ex7 :: Query '[] '[] DB '[]
  '[ "box_code" ::: 'NotNull 'PGtext
   , "city_name" ::: 'NotNull 'PGtext ]
ex7 = select_ (#b ! #code `as` #box_code :* #w ! #location `as` #city_name) 
  (from (table (#boxes `as` #b) 
        & innerJoin (table (#warehouses `as` #w)) (#b ! #warehouse .== #w ! #code)))

-- | 8. Select the warehouse codes, along with the number of boxes in each
-- warehouse. Optionally, take into account that some warehouses are empty
-- (i.e., the box count should show up as zero, instead of omitting the warehouse
-- from the result).
ex8 :: Query '[] '[] DB '[]
  '[ "warehouse_code" ::: 'NotNull 'PGint8
   , "box_count" ::: 'NotNull 'PGint8 ]
ex8 = select_
   (#w ! #code `as` #warehouse_code
 :* count (All $ #b ! #code) `as` #box_count)
   (from (table (#warehouses `as` #w)
         & leftOuterJoin (table (#boxes `as` #b)) (#b ! #warehouse .== #w ! #code))
   & groupBy (#w ! #code))

-- | 9. Select the codes of all warehouses that are saturated (a warehouse is
-- saturated if the number of boxes in it is larger than the warehouse's capacity).
ex9 :: Query '[] '[] DB '[]
  '[ "warehouse_code" ::: 'NotNull 'PGint8 ]
ex9 = select_
   (#w ! #code `as` #warehouse_code)
   (from (table (#warehouses `as` #w)
         & leftOuterJoin (table (#boxes `as` #b)) (#b ! #warehouse .== #w ! #code))
   & groupBy (#w ! #code :* #w ! #capacity)
   & having (count (All $ #b ! #code) .> #w ! #capacity))

-- | 10. Select the codes of all the boxes located in Chicago.
ex10 :: Query '[] '[] DB '[]
  '[ "warehouse_code" ::: 'NotNull 'PGint8 ]
ex10 = select_
  (#w ! #code `as` #warehouse_code)
  (from (table (#warehouses `as` #w))
  & where_ (#w ! #location .== inline "Chicago"))

-- | 11. Create a new warehouse in New York with a capacity for 3 boxes.
ex11 :: Manipulation '[] DB '[] '[]
ex11 = insertInto #warehouses
  (Values_
    (Default `as` #code
  :* Set (inline "New York") `as` #location
  :* Set (inline (3 :: Int64)) `as` #capacity
    )
  )
  OnConflictDoRaise 
  (Returning_ Nil)

-- | 12. Create a new box, with code "H5RT", containing "Papers" with a value
-- of $200, and located in warehouse 2.
ex12 :: Manipulation '[] DB '[] '[]
ex12 = insertInto #boxes
  (Values_
    (Set (inline "H5RT") `as` #code
  :* Set (inline "Papers") `as` #contents
  :* Set (inline (Money 20000)) `as` #value
  :* Set (inline (2 :: Int64)) `as` #warehouse
    )
  )
  OnConflictDoRaise
  (Returning_ Nil)
    
-- | 13. Reduce the value of all boxes by 15%.
ex13 :: Manipulation '[] DB '[] '[]
ex13 = update_ #boxes 
  (Set (money `cast` ((numeric `cast` #value) * 0.85)) `as` #value) true 

-- | 14. Apply a 20% value reduction to boxes with a value larger than the
-- average value of all the boxes.
-- TODO
ex14 :: Manipulation '[] DB '[] '[]
ex14 = with (q `as` #q) _
  where
    q = queryStatement (select_ (avg (All $ #code) `as` #average_box_value) (from (table #boxes) & groupBy Nil))

-- | 15. Remove all boxes with a value lower than $100.
-- TODO
