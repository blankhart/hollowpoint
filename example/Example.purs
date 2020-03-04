module Example where

data DataObject = DataObject
  { hello :: String
  , world :: Object
  }

type Object =
  { hello :: String
  , world :: String
  }

data Datum
  = DatumA { a :: Int, b :: String }
  | DatumB Int String Boolean

newtype Novelty = Novelty String

data Nullary = Nullary

data ProductCtor = ProductCtor String Int

data ProductCtorX = ProductCtorX String Int

update :: Object -> Object
update obj = obj { world = "updated" }

updateSomeName :: forall r. { name :: String | r } -> { name :: String | r }
updateSomeName = _ { name = "blonkhart" }

-- No way to instantiate an open record
-- updateSomeX :: forall r. { name :: String | r }
-- updateSomeX = { name: "blonkhart" }

updateDataObject :: DataObject -> DataObject
updateDataObject (DataObject o) =
  DataObject (o { world = o.world { world = "updated" } })

doMatch :: Datum -> String
doMatch = case _ of
  DatumA { a: 5, b: s } -> s
  DatumA { a, b } -> b
  DatumB i s b -> s

foreign import subtract :: Int -> Int -> Int

tryTCO :: Int -> Int
tryTCO = case _ of
  0 -> 0
  x -> tryTCO (subtract 1 x)
