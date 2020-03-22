module Main where

import Prelude
import Effect (Effect)
import Effect.Print (print)
import Flutter as F
import Other (_underscored)

{-
final myApp = MyApp();

class MyApp extends flutter.StatelessWidget {
  @override
  flutter.Widget build(flutter.BuildContext context) {
    return flutter.MaterialApp(
      title: 'Welcome to Flutter',
      home: flutter.Scaffold(
        appBar: flutter.AppBar(
          title: flutter.Text('Welcome to Flutter'),
        ),
        body: flutter.Center(
          child: flutter.Text('Hello World'),
        ),
      ),
    );
  }
}
-}

myApp :: F.Widget
myApp = F.stateless build unit
  where
    build _ _ = F.materialApp "Welcome to Hollowpoint" home
    home = F.scaffold appbar body
    appbar = F.appBar (F.text "Welcome to Hollowpoint")
    body = F.center (F.text "Hello world!")

data DataObject = DataObject
  { hello :: String
  , world :: Object
  }

_what :: Int
_what = 5

instance showDataObject :: Show DataObject where
  show (DataObject o) = show o

type Object =
  { hello :: String
  , world :: String
  }

data Datum
  = DatumA { a :: Int, b :: String }
  | DatumB Int String Boolean

instance showDatum :: Show Datum where
  show = case _ of
    DatumA _ -> "AltA"
    DatumB _ _ _ -> "AltB"

newtype Novelty = Novelty String

instance showNovelty :: Show Novelty where
  show (Novelty s) = s

data Nullary = Nullary

data ProductCtor = ProductCtor String Int

data ProductCtorX = ProductCtorX String Int

instance showNullary :: Show Nullary where
  show Nullary = "Nullary"

update :: Object -> Object
update obj = obj { world = "updated" }

updateSomeName :: forall r. { name :: String | r } -> { name :: String | r }
updateSomeName = _ { name = "blonkhart" }

-- No way to instantiate an open record
-- updateSomeX :: forall r. { name :: String | r }
-- updateSomeX = { name: "blonkhart" }

updateDataObject :: DataObject -> DataObject
updateDataObject (DataObject o) = DataObject $
  o { world = o.world { world = "updated" } }

doMatch :: Datum -> String
doMatch = case _ of
  DatumA { a: 5, b: s } -> s
  DatumA { a, b } | a == 6 -> b
  DatumB i s b -> s
  _ -> "failed match"

tryTCO :: Int -> Int
tryTCO x = if x < 0 then x else tryTCO (x - 1)

main :: Effect Unit
main = do
  F.runApp myApp
  print $ (show >>> show >>> show) (DatumB 5 "Five" true)
  print $ show (Novelty "newness")
  print $ show Nullary
  let object = { hello : "hiya", world: "globe" }
  print $ show object
  print $ show $ update object
  let dataObject = DataObject { hello: "ho there", world: object }
  print $ show dataObject
  print $ show $ updateDataObject dataObject
  let x = 5
  let y = "Boo"
  case ProductCtor y x of
    ProductCtor a b -> print "Success"
--    ProductCtorX a b -> print "Failure"
  print $ show $ tryTCO 10
  print $ show _what
  let a = 5
      b = 6
  print $ "Eleven: " <> show (5 + 6)
  print "Hello."
  print $ "Underscored: " <> show _underscored

