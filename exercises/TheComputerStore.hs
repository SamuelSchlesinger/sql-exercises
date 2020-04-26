{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module TheComputerStore where

import Data.Int (Int64)
import GHC.TypeLits (Symbol)
import Squeal.PostgreSQL
import Data.Text (pack)
import Shared

type instance Columns "manufacturers" =
  '[ "code" ::: 'Def :=> 'NotNull 'PGint8
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext
   ]

type instance Constraints "manufacturers" =
  '[ "pk_manufacturers" ::: 'PrimaryKey '["code"]
   ]

type instance Columns "products" =
  '[ "code" ::: 'Def :=> 'NotNull 'PGint8
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext
   , "price" ::: 'NoDef :=> 'NotNull 'PGmoney
   , "manufacturer" ::: 'NoDef :=> 'NotNull 'PGint8 
   ]

type instance Constraints "products" =
  '[ "pk_products" ::: 'PrimaryKey '["code"]
   , "fk_manufacturer" ::: 'ForeignKey '["manufacturer"] "manufacturers" '["code"]
   ]

type DB = Public '[ SimpleTable "manufacturers", SimpleTable "products" ]

-- | 1. Select the names of all the products in the store.
ex1 :: Query '[] '[] DB '[]
  '["product_name" ::: 'NotNull 'PGtext]
ex1
  = select_ (#name `as` #product_name) 
    (from (table #products))

-- | 2. Select the names and the prices of all the products in the store.
ex2 :: Query '[] '[] DB '[]
  '["product_name" ::: 'NotNull 'PGtext, "product_price" ::: 'NotNull 'PGmoney]
ex2
  = select_ (#name `as` #product_name
          :* #price `as` #product_price) 
    (from (table #products))

-- | 3. Select the name of the products with a price less than or equal to $200.
ex3 :: Query '[] '[] DB '[] '["product_name" ::: 'NotNull 'PGtext]
ex3
  = select_ (#name `as` #product_name)
    (from (table #products) & where_ (#price .< inline (Money 200)))

-- | 4. Select all the products with a price between $60 and $120.
ex4 :: Query '[] '[] DB '[]
  '[ "product_code" ::: 'NotNull 'PGint8
   , "product_name" ::: 'NotNull 'PGtext
   , "product_price" ::: 'NotNull 'PGmoney
   , "product_manufacturer" ::: 'NotNull 'PGint8
   ]
ex4
  = select_ (#code `as` #product_code
          :* #name `as` #product_name
          :* #price `as` #product_price
          :* #manufacturer `as` #product_manufacturer)
    (from (table #products) & where_ (#price `between` (inline (Money 60), inline (Money 200))))

-- | 5. Select the name and price in cents (i.e., the price must be multiplied by 100).
ex5 :: Query '[] '[] DB '[]
  '[ "product_name" ::: 'NotNull 'PGtext
   , "product_price_in_cents" ::: 'NotNull 'PGint8
   ]
ex5
  = select_ (#name `as` #product_name
          :* ((#price & cast int8) * 2) `as` #product_price_in_cents)
    (from (table #products))

-- | 6. Compute the average price of all the products.
ex6 :: Query '[] '[] DB '[]
  '[ "average_product_price" ::: 'Null 'PGnumeric
   ]
ex6
  = select_ (avg (All (int8 `cast` (#p ! #price))) `as` #average_product_price)
    (from (table (#products `as` #p)) & groupBy Nil)

-- | 7. Compute the average price of all products with manufacturer code equal to 2.
ex7 :: Query '[] '[] DB '[]
  '[ "average_product_price" ::: 'Null 'PGmoney
   ]
ex7
 = select_ (money `cast` avg (All (int8 `cast` (#p ! #price))) `as` #average_product_price)
   (from (table (#products `as` #p)) & where_ (#p ! #manufacturer .== inline (2 :: Int64)) & groupBy Nil)

-- | 8. Compute the number of products with a price larger than or equal to $180.
ex8 :: Query '[] '[] DB '[]
  '[ "product_count" ::: 'NotNull 'PGint8 ]
ex8
  = select_ (countStar `as` #product_count)
    (from (table (#products `as` #p)) & where_ (#p ! #price .>= inline (Money 180)) & groupBy Nil)

-- | 9. Select the name and price of all products with a price larger than or
-- equal to $180, and sort first by price (in descending order), and then by
-- name (in ascending order).
ex9 :: Query '[] '[] DB '[]
  '[ "product_name" ::: 'NotNull 'PGtext
   , "product_price" ::: 'NotNull 'PGmoney
   ]
ex9
  = select_ (#p ! #name `as` #product_name
          :* #p ! #price `as` #product_price)
    (from (table (#products `as` #p)) 
      & where_ (#p ! #price .>= inline (Money 180)) 
      & orderBy [Desc (#p ! #price), Asc (#p ! #name)])

-- | 10. Select all the data from the products, including all the data for
-- each product's manufacturer.
ex10 :: Query '[] '[] DB '[]
  '[ "product_code" ::: 'NotNull 'PGint8
   , "product_name" ::: 'NotNull 'PGtext
   , "product_price" ::: 'NotNull 'PGmoney
   , "product_manufacturer" ::: 'NotNull 'PGint8
   , "product_manufacturer_name" ::: 'NotNull 'PGtext
   ]
ex10
  = select_ (#p ! #code `as` #product_code
          :* #p ! #name `as` #product_name
          :* #p ! #price `as` #product_price
          :* #p ! #manufacturer `as` #product_manufacturer
          :* #m ! #name `as` #product_manufacturer_name)
    (from (table (#products `as` #p)
          & innerJoin (table (#manufacturers `as` #m)) (#m ! #code .== #p ! #manufacturer)))

-- | 11. Select the product name, price, and manufacturer name of all the products.
ex11 :: Query '[] '[] DB '[]
  '[ "product_name" ::: 'NotNull 'PGtext
   , "product_price" ::: 'NotNull 'PGmoney
   , "product_manufacturer_name" ::: 'NotNull 'PGtext
   ]
ex11
  = select_ (#p ! #name `as` #product_name
          :* #p ! #price `as` #product_price
          :* #m ! #name `as` #product_manufacturer_name)
    (from (table (#products `as` #p)
          & innerJoin (table (#manufacturers `as` #m)) (#p ! #manufacturer .== #m ! #code)))

-- | 12. Select the average price of each manufacturer's products, showing only
-- the manufacturer's code.
ex12 :: Query '[] '[] DB '[]
  '[ "manufacturer_code" ::: 'NotNull 'PGint8
   , "average_product_price" ::: Null 'PGmoney ]
ex12
 = select_ (#m ! #code `as` #manufacturer_code
         :* money `cast` avg (All (int8 `cast` (#p ! #price))) `as` #average_product_price)
    (from (table (#manufacturers `as` #m)
                  & leftOuterJoin (table (#products `as` #p)) (#p ! #manufacturer .== #m ! #code))
     & groupBy (#m ! #code))

-- | 13. Select the average price of each manufacturer's products, showing the manufacturer's name.
ex13 :: Query '[] '[] DB '[]
  '[ "manufacturer_name" ::: 'NotNull 'PGtext
   , "average_product_price" ::: 'NotNull 'PGmoney ]
ex13
  = select_ (#m ! #name `as` #manufacturer_name
          :* money `cast` avg (All (int8 `cast` (#p ! #price))) `as` #average_product_price)
    (from (table (#manufacturers `as` #m)
                & leftOuterJoin (table (#products `as` #p)) (#m ! #code .== #p ! #manufacturer))
    & groupBy (#m ! #code :* #m ! #name))

-- | 14. Select the names of manufacturer whose products have an average price
-- larger than or equal to $150.
ex14 :: Query '[] '[] DB '[]
  '[ "manufacturer_name" ::: 'NotNull 'PGtext ]
ex14
  = select_ (#m ! #name `as` #manufacturer_name)
    (from (table (#manufacturers `as` #m)
                & leftOuterJoin (table (#products `as` #p)) (#p ! #manufacturer .== #m ! #code))
     & groupBy (#m ! #code :* #m ! #name)
     & having (money `cast` avg (All (int8 `cast` (#p ! #price))) .>= inline (Money 150) ))

-- | 15. Select the name and price of the cheapest product.
ex15 :: Query '[] '[] DB '[]
  '[ "product_name" ::: 'NotNull 'PGtext
   , "product_price" ::: 'NotNull 'PGmoney ]
ex15
  = select_ (#p ! #name `as` #product_name
          :* #p ! #price `as` #product_price)
    (from (table (#products `as` #p))
    & orderBy [Asc $ #p ! #price]
    & limit 1)

-- | 16. Select the name of each manufacturer along with the name and price of its most expensive product.
ex16 :: Query '[] '[] DB '[]
  '[ "manufacturer_name" ::: 'NotNull 'PGtext
   , "product_price" ::: 'Null 'PGmoney 
   , "product_name" ::: 'Null 'PGtext ]
ex16
  = with (q `as` #q)
    (select_ (#q ! #m_name `as` #manufacturer_name
          :* #q ! #p_price `as` #product_price
          :* #p ! #name `as` #product_name)
    (from (common #q 
      & leftOuterJoin 
          (table (#products `as` #p)) 
          (#p ! #manufacturer .== #q ! #m_code .&& #p ! #price .== #q ! #p_price))))
  where
    q :: Query '[] '[] DB '[]
      '[ "m_code" ::: 'NotNull 'PGint8
       , "m_name" ::: 'NotNull 'PGtext
       , "p_price" ::: 'Null 'PGmoney
       ]
    q = select_ (#m ! #code `as` #m_code
              :* #m ! #name `as` #m_name
              :* max_ (allNotNull $ #p ! #price) `as` #p_price)
        (from (table (#manufacturers `as` #m)
                   & leftOuterJoin (table (#products `as` #p)) (#p ! #manufacturer .== #m ! #code))
        & groupBy (#m ! #code :* #m ! #name))

-- | 17. Add a new product: Loudspeakers, $70, manufacturer 2.
ex17 :: Manipulation '[] DB '[] '[]
ex17 = insertInto_ #products 
        (Values_
          (Default `as` #code
        :* Set (inline "Loudspeakers") `as` #name
        :* Set (inline (Money 7000)) `as` #price
        :* Set (inline (2 :: Int64)) `as` #manufacturer)
        )

-- | 18. Update the name of product 8 to "Laser Printer".
ex18 :: Manipulation '[] DB '[] '[]
ex18 = update_ #products
        (Set (inline "Laser Printer") `as` #name)
        (#code .== 2)

-- | 19. Apply a 10% discount to all products.
ex19 :: Manipulation '[] DB '[] '[]
ex19 = update_ #products
        (Set (money `cast` ((numeric `cast` #price) * 0.9)) `as` #price)
        true

-- | 20. Apply a 10% discount to all products with a price larger than or equal to $120.
ex20 :: Manipulation '[] DB '[] '[]
ex20 = update_ #products
        (Set (money `cast` ((numeric `cast` #price) * 0.9)) `as` #price)
        (#price .>= inline (Money 120))

setup :: Migration Definition (Public '[]) DB
setup = Migration
  { name = pack "TheComputerStore"
  , migration = up
  } where
      up :: Definition (Public '[]) DB
      up = createTable #manufacturers
           (serial8 `as` #code
         :* notNullable text `as` #name)
           (primaryKey #code `as` #pk_manufacturers)
       >>> createTable #products
           (serial8 `as` #code
         :* notNullable text `as` #name
         :* notNullable money `as` #price
         :* notNullable int8 `as` #manufacturer)
           (primaryKey #code `as` #pk_products
         :* foreignKey #manufacturer #manufacturers #code OnDeleteCascade OnUpdateCascade `as` #fk_manufacturer)
