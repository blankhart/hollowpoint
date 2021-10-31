module Language.PureScript.CodeGen.Dart where

import Options.Applicative (execParser)

import Language.PureScript.CodeGen.Dart.Command.Compile (compile)
import Language.PureScript.CodeGen.Dart.Command.Options (cliParser)
-- import Language.PureScript.CodeGen.Dart.Version (versionString)

main :: IO ()
main = do
  putStrLn "Hollowpoint running..."
  opts <- execParser cliParser
  compile opts
  putStrLn "Hollowpoint done."
