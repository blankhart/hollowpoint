module Example where

-- Example sum/product type data declaration.
data ExampleSumProduct
  = ExampleTermA Int Boolean
  | ExampleTermB String Number

-- Example nullary data constructor.
data ExampleNullary = ExampleNullary

-- Example newtype
newtype ExampleNewtypeInt = ExampleNewtypeInt Int

-- Example newtype over record
newtype ExampleNewtypeRecord = ExampleNewtypeRecord
  { hello :: String
  , world :: String
  }

-- Type synonym
type ExampleTypeSynonym =
  { hello :: String
  , world :: String
  }

class ExampleTypeclass a where
  getNullary1 :: a -> ExampleNullary
  getNullary2 :: a -> a -> ExampleNullary

instance etNewtypeInt :: ExampleTypeclass ExampleNewtypeInt where
  getNullary1 _ = ExampleNullary
  getNullary2 _ _ = ExampleNullary

-- Closed record update
updateRecordClosed :: ExampleTypeSynonym -> ExampleTypeSynonym
updateRecordClosed hw = hw { world = "updated" }

-- Open record update
updateRecordOpen :: forall r. { hello :: String | r } -> { hello :: String | r }
updateRecordOpen = _ { hello = "hi" }

-- Case statement
doCaseMatch :: ExampleSumProduct -> String
doCaseMatch = case _ of
  ExampleTermA _ _ -> "A"
  ExampleTermB _ _ -> "B"

foreign import subtract :: Int -> Int -> Int

doTCO :: Int -> Int
doTCO = case _ of
  0 -> 0
  x -> doTCO (subtract 1 x)
