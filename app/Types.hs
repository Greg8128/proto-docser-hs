{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where
import Data.Aeson
import GHC.Generics

type Version = String

data Metadata = Metadata
  {formatVersion :: Version,
   ghcVersion :: Version,
   haddockVersion :: Version}
              deriving (Generic, ToJSON)
