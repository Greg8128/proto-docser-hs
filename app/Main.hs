{-#LANGUAGE NoMonomorphismRestriction #-}

module Main where
import GHC
import Control.Monad.IO.Class
import GHC.Core
import GHC.Paths
import Language.Haskell.GHC.ExactPrint.Types
import EncodeDocument as ED
import Data.Aeson as Aeson
import Data.ByteString.Lazy as BS
import Types
import GHC.Unit.Module.Graph (summaryNodeSummary)


main :: IO ()
main = do
  let src = "example/Example.hs"
      out = "example/Example.json"
      modname = "Example"
  putStrLn $ "Serializing module " <> modname <> " at " <> " src " <> " to " <> out
  serializeModule src modname out

serializeModule src modname out = runGhc (Just libdir) $ do
  session <- getSession

  -- the line below prevents the following error:
  -- proto-docser: panic! (the 'impossible' happened)
  -- GHC version 9.4.5:
  --       unsafeGetHomeUnit: No home unit
  setSessionDynFlags =<< getSessionDynFlags

  target <- guessTarget src Nothing Nothing
  setTargets [target]
  load LoadAllTargets
  modSummary <- getModSummary (mkModuleName modname)
  parsedMod <- parseModule modSummary
  typecheckedMod <- typecheckModule parsedMod
  let Just rn_source = tm_renamed_source typecheckedMod
      metadata = Metadata{formatVersion = "0.0", ghcVersion = "9.4.5", haddockVersion = "2.25.0"}
      encoded = Aeson.encode (ED.encode metadata rn_source)
  liftIO $ BS.writeFile out encoded
