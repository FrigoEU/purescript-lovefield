module Lovefield where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Promise (Promise, toAff)
import Data.Array (snoc)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class AsForeign, write)
import Data.StrMap (StrMap, empty, insert)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Prelude (class Show, Unit, (#), (<$>), (>=>), (>>>))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class HasField l s a | l s -> a where
  getField :: SProxy l -> s -> a

data HNil = HNil

foreign import data HCons :: Symbol -> * -> * -> *
foreign import dynGet :: forall s a. String -> s -> a
foreign import showHCons :: forall a b c. HCons a b c -> String

foreign import data LF :: !

instance showHNil :: Show HNil where
  show _ = "HNil"

instance showHConsInstance :: (Show a, Show b) => Show (HCons l a b) where
  show = showHCons

instance hasfieldFail ::
  Fail ("Missing field \"" <> l <> "\" of type " <> TypeString a)
                         => HasField l HNil a where
  getField _ = unsafeCoerce
infixl 6 type TypeConcat as <>

instance hasFieldHead :: IsSymbol l => HasField l (HCons l head tail) head where
  getField l s = dynGet (reflectSymbol l) s

-- Is proberen: misschien moeten we head in (Column a) veranderen ofzoiets, zodat we fields van hetzelfde type hebben?
instance hasFieldTail ::
  (IsSymbol l, IsSymbol l1, HasField l tail a)
  => HasField l (HCons l1 head tail) a where
  getField l s = dynGet (reflectSymbol l) s

hcons :: forall a l rest. (IsSymbol l) => (SProxy l) -> a -> rest -> HCons l a rest
hcons sp = hconsImpl (reflectSymbol sp)
foreign import hconsImpl :: forall a l rest. String -> a -> rest -> HCons l a rest

-- TODO overal zit de assumptie dat HCons bestaat uit aalemaal HCons'en. Afdwingen!
foreign import getLabels :: forall l a rest. HCons l a rest -> Array String

-- Tables & Columns
data Table (name :: Symbol) fields = Table (SProxy name) fields
data Column a = Column
newtype TableName = TableName String
newtype ColumnName = ColumnName String
newtype Operator = Operator String

getTableName :: forall name fields. (IsSymbol name) => Table name fields -> String
getTableName (Table sp _) = reflectSymbol sp

-- An example table
-- products :: Table "products" (HCons "id" (Column Int)
--                              (HCons "price" (Column Int)
--                               HNil))
-- products = Table SProxy (hcons SProxy Column
--                         (hcons SProxy Column HNil))

-- orders :: Table "orders" (HCons "id" (Column Int)
--                          (HCons "productId" (Column Int)
--                          (HCons "amount" (Column Int)
--                           HNil)))
-- orders = Table SProxy (hcons SProxy Column
--                       (hcons SProxy Column
--                       (hcons SProxy Column HNil)))

data Selection (tn :: Symbol) fields selected = Selection TableName (Array ColumnName)
-- Todo model projections as NonEmptyList
-- Tracking tables with all their fields and the selections seperately in the types
-- so we can join and filter on all fields, even when they're not being selected
newtype Query tables selection = Query { projections :: Array { tn :: TableName
                                                              , cns :: Array ColumnName}
                                       , predicates :: Array { op :: Operator
                                                             , tn :: TableName
                                                             , cn :: ColumnName
                                                             , v :: Foreign}
                                       , joins :: Array { tn1 :: TableName
                                                        , cn1 :: ColumnName
                                                        , tn2 :: TableName
                                                        , cn2 :: ColumnName}
                                       }

-- NOTE:
-- order of tables in from and join is important!
-- from A, then join B.col on A.col!
-- so snoc is the right operator

joinOn ::
  forall t1 cs1 l1 t2 cs2 l2 a ts sel sel2.
  (HasField l1 cs1 (Column a), HasField l2 cs2 (Column a), IsSymbol l1, IsSymbol l2, IsSymbol t1, IsSymbol t2, HasField t1 ts cs1) =>
  Table t1 cs1 -> SProxy l1 -> Table t2 cs2 -> SProxy l2 -> Selection t2 cs2 sel2 -> Query ts sel -> Query (HCons t2 cs2 ts) (HCons t2 sel2 sel)
joinOn (Table t1 cs1) l1 (Table t2 cs2) l2 (Selection _ sel) (Query q) =
  Query (q { joins = q.joins `snoc` { tn1: TableName (reflectSymbol t1)
                                    , cn1: ColumnName (reflectSymbol l1)
                                    , tn2: TableName (reflectSymbol t2)
                                    , cn2: ColumnName (reflectSymbol l2)
                                    }
           , projections = q.projections `snoc` { tn: TableName (reflectSymbol t2)
                                                , cns: sel}
             })

-- TODO IsSQLValue hier gebruiken ipv AsForeign?
where' ::
  forall tl ts cs cl a b.
  (HasField tl ts cs, HasField cl cs (Column a), AsForeign a, IsSymbol tl, IsSymbol cl) =>
  Table tl cs -> SProxy cl -> Operator -> a -> Query ts b -> Query ts b
where' (Table tl _) cl op a (Query q)=
  Query (q {predicates = q.predicates `snoc` { op: op
                                             , tn: TableName (reflectSymbol tl)
                                             , cn: ColumnName (reflectSymbol cl)
                                             , v: write a
                                             }})

-- TODO rename to "from"??
emptySelection :: forall t cs. (IsSymbol t) => Table t cs -> Selection t cs HNil
emptySelection (Table t _) = Selection (TableName (reflectSymbol t)) []

-- TODO unsafe!!
selectAll :: forall t l a rest. (IsSymbol t, IsSymbol l) => Table t (HCons l a rest) -> Selection t (HCons l a rest) (HCons l a rest)
selectAll (Table t fields) = Selection (TableName (reflectSymbol t)) (ColumnName <$> getLabels fields)

project ::
  forall t l cs a cssel.
  (HasField l cs (Column a), IsSymbol t, IsSymbol l) =>
  SProxy l -> Selection t cs cssel -> Selection t cs (HCons l a cssel)
project l (Selection t fs) = Selection t (fs `snoc` (ColumnName (reflectSymbol l)))

select :: forall t cs sel.
          (IsSymbol t) =>
          Selection t cs sel -> Query (HCons t cs HNil) (HCons t sel HNil)
select (Selection t cs) =
  Query { projections: [{tn: t, cns: cs}]
        , predicates: []
        , joins: []
        }

-- applySelect ::
--   forall t ts cs sc sel1 sel2.
--   (HasField t ts sc, IsSymbol t) =>
--   Query ts sel1 -> Selection t cs sel2 -> Query ts (HCons t sel2 sel1)
-- applySelect (Query q) (Selection tn cs)=
--   Query (q {projections = q.projections `snoc` (Tuple tn cs)})

data Insertion (tablename :: Symbol) values = Insertion TableName (StrMap Foreign)
insertInto :: forall t cols. (IsSymbol t) => Table t cols -> Insertion t cols
insertInto (Table t _) = Insertion (TableName (reflectSymbol t)) empty

value :: forall t l a rest. (IsSymbol l, AsForeign a) =>
         SProxy l -> a -> Insertion t (HCons l (Column a) rest) -> Insertion t rest
value l a (Insertion t values) = Insertion t (insert (reflectSymbol l) (write a) values)

-- TODO optimize API + for performance
foreign import data LFDB :: *
foreign import data LFSchema :: *
foreign import data LFTable :: *
foreign import connectImpl :: forall e. SchemaBuilder -> Eff (lf :: LF | e) (Promise LFDB)

connect :: forall e. SchemaBuilder -> Aff (lf :: LF | e) LFDB
connect = connectImpl >>> liftEff >=> toAff

foreign import getSchema :: LFDB -> LFSchema
foreign import getTable :: LFSchema -> String -> LFTable -- TODO what if table doesn't exist?
foreign import insertImpl ::
  forall e. LFDB -> LFTable -> StrMap Foreign -> Eff (lf :: LF | e) (Promise Unit)

runInsert :: forall t e. LFDB -> Insertion t HNil -> Aff (lf :: LF | e) Unit
runInsert db (Insertion (TableName tn) values) = 
  insertImpl db (getTable (getSchema db) tn) values # (liftEff >=> toAff)

foreign import data SchemaBuilder :: *
foreign import buildSchema :: forall e. String -> Int -> Eff (lf :: LF | e) SchemaBuilder
-- TODO deleteDB: only indexedDB, not sure what to do about it
foreign import deleteDatabase :: forall e. String -> Eff (lf :: LF, err :: EXCEPTION | e) Unit
data Creation (tablename :: Symbol) values = Creation TableName (StrMap Int) (Array String)
-- ^ StrMap Int = cols with type enum
-- ^ Array String = Primary Key
createTable :: forall t cols. (IsSymbol t) => Table t cols -> Creation t cols
createTable (Table t _) = Creation (TableName (reflectSymbol t)) empty []

-- TODO kinda annoying that we have to run over all labels again, but I don't know how/if we can iterate over HCons
-- TODO do we need this Monoid + mempty stuff here? Couldn't figure out how to get ToSqlEnum to work otherwise
addColumn :: forall t l a rest. (IsSymbol l, ToSqlEnum a) =>
             SProxy l -> Boolean -> Creation t (HCons l (Column a) rest) -> Creation t rest
addColumn l pk (Creation t cols pks) =
  Creation
    t
    (insert (reflectSymbol l) (toSqlEnum (Proxy :: Proxy a)) cols)
    (if pk then pks `snoc` (reflectSymbol l) else pks)

class ToSqlEnum a where toSqlEnum :: Proxy a -> Int

-- instance toSqlEnumArrayBuffer :: ToSqlEnum ArrayBuffer where toSqlEnum _ = 0
instance toSqlEnumBoolean :: ToSqlEnum Boolean where toSqlEnum _ = 1
-- instance toSqlEnumDatetime :: ToSqlEnum Datetime where toSqlEnum _ = 2
instance toSqlEnumInt :: ToSqlEnum Int where toSqlEnum _ = 3
instance toSqlEnumNumber :: ToSqlEnum Number where toSqlEnum _ = 4
instance toSqlEnumString :: ToSqlEnum String where toSqlEnum _ = 5
-- instance toSqlEnumObject :: ToSqlEnum Object where toSqlEnum _ = 6

-- TODO support indexes
-- TODO we're not enforcing a primary key to be present...
foreign import createImpl :: forall e. SchemaBuilder -> String -> StrMap Int -> Array String -> Eff (lf :: LF | e) Unit
runCreation :: forall t e. SchemaBuilder -> Creation t HNil -> Eff (lf :: LF | e) Unit
runCreation sb (Creation (TableName tn) cols pk) = createImpl sb tn cols pk

foreign import runQueryImpl ::
  forall ts sel e. LFDB -> Query ts sel -> Eff (lf :: LF | e) (Promise (Array sel))
runQuery :: forall ts sel e. LFDB -> Query ts sel -> Aff (lf :: LF | e) (Array sel)
runQuery db q = runQueryImpl db q # (liftEff >=> toAff)

-- TODO more operators
equal :: Operator
equal = Operator "eq"

-- TODO AND/OR for predicates. Now more than one predicate -> AND
-- TODO LEFT OUTER JOIN!!
-- TODO Maybe support joins with something else than "eq" ?
-- TODO custom serialization support
-- TODO GRoup by?
-- TODO Limit & Skip
-- TODO observers
-- TODO add exception effects
