module Test.Main where

import Prelude
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Symbol (SProxy(..))
import Debug.Trace (traceAnyA)
import Lovefield (class HasField, Column(Column), HCons, HNil(HNil), LF, LFDB, SchemaBuilder, Table(Table), addColumn, buildSchema, connect, createTable, deleteDatabase, emptySelection, equal, getField, hcons, insertInto, joinOn, project, runCreation, runInsert, runQuery, select, value, where')

products :: Table "products" (HCons "id" (Column Int)
                             (HCons "name" (Column String)
                             (HCons "price" (Column Number)
                             HNil)))
products = Table SProxy (hcons SProxy Column
                        (hcons SProxy Column
                        (hcons SProxy Column HNil)))

orders :: Table "orders" (HCons "id" (Column Int)
                         (HCons "productId" (Column Int)
                         (HCons "amount" (Column Int)
                          HNil)))
orders = Table SProxy (hcons SProxy Column
                      (hcons SProxy Column
                      (hcons SProxy Column HNil)))

createProducts :: forall e. SchemaBuilder -> Eff ( lf :: LF | e) Unit
createProducts sb =
  createTable products
  # addColumn (SProxy :: SProxy "id") true
  # addColumn (SProxy :: SProxy "name") false
  # addColumn (SProxy :: SProxy "price") false
  # runCreation sb

-- Things to note:
-- * labels needs to be ordered
-- * Missing a field -> (hard to read, but OK) Type Error
-- * Pretty easy to read
createOrders :: forall e. SchemaBuilder -> Eff ( lf :: LF | e) Unit
createOrders sb =
  createTable orders
  # addColumn (SProxy :: SProxy "id") true
  # addColumn (SProxy :: SProxy "productId") false
  # addColumn (SProxy :: SProxy "amount") false
  # runCreation sb

-- Things to note:
-- * labels needs to be ordered
-- * Missing a field -> (hard to read, but OK) Type Error
-- * Type safe insert
-- * Pretty easy to read
insertProduct :: forall e. LFDB -> Int -> String -> Number -> Aff (lf :: LF | e) Unit
insertProduct db id name price =
  insertInto products
  # value (SProxy :: SProxy "id") id
  # value (SProxy :: SProxy "name") name
  # value (SProxy :: SProxy "price") price
  # runInsert db

insertOrder :: forall e. LFDB -> Int -> Int -> Int -> Aff (lf :: LF | e) Unit
insertOrder db id productId amount =
  insertInto orders
  # value (SProxy :: SProxy "id") id
  # value (SProxy :: SProxy "productId") productId
  # value (SProxy :: SProxy "amount") amount
  # runInsert db

-- TODO : Can we get rid of this overlapping instances stuff?
-- Is this maybe a problem in the compiler itself?? Can't figure it out.
selectProductById :: forall e. LFDB -> Int -> Aff ( lf :: LF | e) (Array 
                               (HCons "products"
                                 (HCons "price" Number
                                 (HCons "name" String
                                 (HCons "id" Int HNil))) HNil))
selectProductById db id =
  select (emptySelection products
          # project (SProxy :: SProxy "id")
          # project (SProxy :: SProxy "name")
          # project (SProxy :: SProxy "price"))
  # where' products (SProxy :: SProxy "id") equal id
  # runQuery db

-- "order" of tables is reversed :(
selectProductsAndOrdersByAmount ::
  forall e. LFDB -> Int -> Aff ( lf :: LF | e) (Array
                               (HCons "orders" (HCons "amount" Int
                                               (HCons "productId" Int
                                               (HCons "id" Int HNil)))
                               (HCons "products" (HCons "price" Number
                                                 (HCons "name" String
                                                 (HCons "id" Int HNil))) HNil)))
selectProductsAndOrdersByAmount db amount =
  select (emptySelection products
          # project (SProxy :: SProxy "id")
          # project (SProxy :: SProxy "name")
          # project (SProxy :: SProxy "price"))
  # joinOn products (SProxy :: SProxy "id")
         orders (SProxy :: SProxy "productId")
        (emptySelection orders
          # project (SProxy :: SProxy "id")
          # project (SProxy :: SProxy "productId")
          # project (SProxy :: SProxy "amount"))
  # where' orders (SProxy :: SProxy "amount") equal amount
  # runQuery db

_products :: forall a b. (HasField "products" a b) => a -> b
_products = getField (SProxy :: SProxy "products")

_name :: forall a b. (HasField "name" a b) => a -> b
_name = getField (SProxy :: SProxy "name")

main :: forall e. Eff (lf :: LF, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = void $ runAff (show >>> log) (\_ -> log "Done!") do
  sb <- liftEff do
    deleteDatabase "lovefieldtest"
    sb <- buildSchema "lovefieldtest" 1
    createProducts sb
    createOrders sb
    pure sb

  db <- connect sb
  insertProduct db 1 "spaghetti" 8.99
  insertProduct db 2 "pizza" 12.99
  insertProduct db 3 "lasagna" 15.99
  insertOrder db 1 1 4
  insertOrder db 2 2 4
  insertOrder db 3 2 15
  insertOrder db 4 3 1

  selectProductById db 2 >>= map (_products >>> _name) >>> traceAnyA
    -- Almost lenses!
    -- Should be ["pizza"]

  selectProductsAndOrdersByAmount db 4 >>= map (_products >>> _name) >>> traceAnyA
    -- Should be ["spaghetti", "pizza"]
