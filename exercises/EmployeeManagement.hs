{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module EmployeeManagement where

import Shared
import Squeal.PostgreSQL
import Data.Int (Int64)

type instance Columns "departments" =
  '[ "code" ::: 'Def :=> 'NotNull 'PGint8
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext
   , "budget" ::: 'NoDef :=> 'NotNull 'PGmoney ]

type instance Constraints "departments" =
  '[ "pk_departments" ::: 'PrimaryKey '["code"] ]

type instance Columns "employees" =
  '[ "ssn" ::: 'NoDef :=> 'NotNull 'PGint8 
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext
   , "last_name" ::: 'NoDef :=> 'NotNull 'PGtext
   , "department" ::: 'NoDef :=> 'NotNull 'PGint8 ]

type instance Constraints "employees" =
  '[ "pk_employees" ::: 'PrimaryKey '["ssn"]
   , "fk_department" ::: 'ForeignKey '["department"] "departments" '["code"] ]

type DB = Public '[ SimpleTable "departments", SimpleTable "employees" ]

-- | 1. Select the last name of all employees.
ex1 :: Query '[] '[] DB '[] '[ "last_name" ::: 'NotNull 'PGtext ]
ex1 = select_ #last_name (from (table #employees))

-- | 2. Select the last name of all employees, without duplicates.
ex2 :: Query '[] '[] DB '[] '[ "last_name" ::: 'NotNull 'PGtext ]
ex2 = selectDistinct_ #last_name (from (table #employees))

-- | 3. Select all of the data of employees whose last name is "Smith".
ex3 :: Query '[] '[] DB '[]
  '[ "ssn" ::: 'NotNull 'PGint8
   , "name" ::: 'NotNull 'PGtext
   , "last_name" ::: 'NotNull 'PGtext
   , "department" ::: 'NotNull 'PGint8 ]
ex3 = select_ (#ssn :* #name :* #last_name :* #department) 
      (from (table #employees) & where_ (#name .== inline "Smith"))

-- | 4. Select all the data of employees whose last name is "Smith" or
-- "Doe"
ex4 :: Query '[] '[] DB '[]
  '[ "ssn" ::: 'NotNull 'PGint8
   , "name" ::: 'NotNull 'PGtext
   , "last_name" ::: 'NotNull 'PGtext
   , "department" ::: 'NotNull 'PGint8 ]
ex4 = select_ (#ssn :* #name :* #last_name :* #department)
      (from (table #employees) & where_ (#name .== inline "Smith" .|| #name .== inline "Doe"))

-- | 5. Select all the data of employees that work in department 14.
ex5 :: Query '[] '[] DB '[] 
  '[ "ssn" ::: 'NotNull 'PGint8
   , "name" ::: 'NotNull 'PGtext
   , "last_name" ::: 'NotNull 'PGtext
   , "department" ::: 'NotNull 'PGint8 ]
ex5 = select_ (#ssn :* #name :* #last_name :* #department)
              (from (table #employees) & where_ (#department .== inline (14 :: Int64))) 

-- | 6. Select all the data of employees that work in department 37 or department 77.
ex6 :: Query '[] '[] DB '[]
  '[ "ssn" ::: 'NotNull 'PGint8
   , "name" ::: 'NotNull 'PGtext
   , "last_name" ::: 'NotNull 'PGtext
   , "department" ::: 'NotNull 'PGint8 ]
ex6 = select_ (#ssn :* #name :* #last_name :* #department)
              (from (table #employees) & where_ (#department .== inline (37 :: Int64)))

-- | 7. Select all the data of employees whose last name begins with an "S".
ex7 :: Query '[] '[] DB '[]
  '[ "ssn" ::: 'NotNull 'PGint8
   , "name" ::: 'NotNull 'PGtext
   , "last_name" ::: 'NotNull 'PGtext
   , "department" ::: 'NotNull 'PGint8 ]
ex7 = select_ (#ssn :* #name :* #last_name :* #department)
              (from (table #employees) & where_ (#last_name `like` inline "S%"))

-- | 8. Select the sum of all the departments' budgets.
ex8 :: Query '[] '[] DB '[]
  '[ "sum_of_budgets" ::: 'Null 'PGmoney ]
ex8 = select (sum_ (All $ #d ! #budget) `as` #sum_of_budgets)
             (from (table (#departments `as` #d)) & groupBy (#d ! #code))

-- | 9. Select the number of employees in each department (you only need to
-- show the department code and the number of employees).
ex9 :: Query '[] '[] DB '[]
  '[ "number_of_employees" ::: 'NotNull 'PGint8
   , "department_code" ::: 'NotNull 'PGint8 ]
ex9 = with (q `as` #q) 
      (select_ ( count (All $ #e ! #ssn) `as` #number_of_employees
              :* #q ! #department_code `as` #department_code )
      (from (common #q
          & leftOuterJoin (table (#employees `as` #e)) (#e ! #department .== #q ! #department_code))
      & groupBy (#q ! #department_code))
      ) where
  q = select (#code `as` #department_code) (from (table #departments))

-- | 10. Select all the data of employees, including each employee's department's data.
ex10 :: Query '[] '[] DB '[]
  '[ "ssn" ::: 'NotNull 'PGint8
   , "name" ::: 'NotNull 'PGtext
   , "last_name" ::: 'NotNull 'PGtext
   , "department_code" ::: 'NotNull 'PGint8 
   , "department_name" ::: 'NotNull 'PGtext
   , "department_budget" ::: 'NotNull 'PGmoney ]
ex10 = select_ (#e ! #ssn `as` #ssn
             :* #e ! #name `as` #name
             :* #e ! #last_name `as` #last_name
             :* #e ! #department `as` #department_code
             :* #d ! #name `as` #department_name
             :* #d ! #budget `as` #department_budget)
       (from (table (#employees `as` #e)
             & innerJoin (table (#departments `as` #d)) (#d ! #code .== #e ! #department)))

-- | 11. Select the name and last name of each employee, along with the name and
-- budget of the employee's department.
ex11 :: Query '[] '[] DB '[]
  '[ "employee_name" ::: 'NotNull 'PGtext
   , "employee_last_name" ::: 'NotNull 'PGtext
   , "department_name" ::: 'NotNull 'PGtext
   , "department_budget" ::: 'NotNull 'PGmoney ]
ex11 = select_ (#e ! #name `as` #employee_name
             :* #e ! #last_name `as` #employee_last_name
             :* #d ! #name `as` #department_name
             :* #d ! #budget `as` #department_budget)
       (from (table (#employees `as` #e)
             & innerJoin (table (#departments `as` #d)) (#d ! #code .== #e ! #department)))

-- | 12. Select the name and last name of employees working for departments with a budget greater than $60,000.
ex12 :: Query '[] '[] DB '[]
  '[ "first_name" ::: 'NotNull 'PGtext
   , "last_name" ::: 'NotNull 'PGtext ]
ex12 = select_ (#e ! #name `as` #first_name :* #e ! #last_name `as` #last_name)
  (from (table (#employees `as` #e)
        & innerJoin (table (#departments `as` #d)) (#d ! #code .== #e ! #department))
  & where_ (#d ! #budget .> inline (Money 6000000)))

-- | 13. Select the departments with a budget larger than the average budget of all the departments.
ex13 :: Query '[] '[] DB '[]
  '[ "code" ::: 'NotNull 'PGint8
   , "name" ::: 'NotNull 'PGtext
   , "budget" ::: 'NotNull 'PGmoney ]
ex13 = with (q `as` #q)
  (select_ (#d ! #code `as` #code 
         :* #d ! #name `as` #name
         :* #d ! #budget `as` #budget)
    (from (common #q
          & innerJoin (table (#departments `as` #d)) true)
    & where_ (#d ! #budget .> #q ! #average_budget)))
  where
    q = select_ ((money `cast` avg (All $ money `cast` (#d ! #budget))) `as` #average_budget)
      (from (table (#departments `as` #d))
      & groupBy (#d ! #code))

-- | 14. Select the names of departments with more than two employees.
ex14 :: Query '[] '[] DB '[]
  '[ "department_name" ::: 'NotNull 'PGtext ]
ex14 = with (q `as` #q)
  (select_ (#d ! #name `as` #department_name)
    (from (common #q
          & innerJoin (table (#departments `as` #d)) (#d ! #code .== #q ! #department_code))
    & where_ (#q ! #employee_count .> 2)))
  where
    q = select_ (#d ! #code `as` #department_code
             :* count (All $ #e ! #ssn) `as` #employee_count)
             (from (table (#departments `as` #d)
                   & leftOuterJoin (table (#employees `as` #e)) (#e ! #department .== #d ! #code))
             & groupBy (#d ! #code))

-- | 15. Select the name and last name of employees working for departments with second lowest budget.
-- TODO
