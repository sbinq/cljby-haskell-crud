module Utils
  ( aesonOptions
  ) where

import           Data.Aeson.TH (Options (..), defaultOptions)
import           Data.Char     (toLower)


aesonOptions :: String -> Options
aesonOptions prefix =
  defaultOptions {
    fieldLabelModifier = lowercaseFirst . drop len
  }
  where len = length prefix

lowercaseFirst :: String -> String
lowercaseFirst []       = []
lowercaseFirst (c:rest) = toLower c : rest
