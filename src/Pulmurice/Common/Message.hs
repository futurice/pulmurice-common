-----------------------------------------------------------------------------
-- |
-- Module      :  Pulmurice.Common.Message
-- Copyright   :  2014 © Futurice OY, Oleg Grenrus
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability   :  experimental
-- Portability :  non-portable (GHC only)
--
-- Request and respond messages.
--
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Pulmurice.Common.Message (ReqMsg(..), ResMsg(..)) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text as T
import Data.Word
import Test.QuickCheck

import Pulmurice.Common.Uniq

-- | Request messages.
data ReqMsg = EchoReqMsg Text
            | PuzzlesReqMsg
            | SignupReqMsg Text Text       -- ^ name, email
            | NewReqMsg Uniq String Word16 -- ^ team-Uniq, puzzle-name, difficulty
            | ListReqMsg Uniq              -- ^ team-Uniq
            | ShowReqMsg Uniq Uniq         -- ^ team-Uniq, puzzle-Uniq
            | SolveReqMsg Uniq Uniq String -- ^ team-Uniq, puzzle-Uniq, solution
            | ErrorReqMsg String           -- ^ message
 deriving (Show, Eq)

encodeMessage :: Text -> Text -> [Pair] -> Value
encodeMessage r t pairs = object $ "r" .= r : "type" .= t : pairs

encodeReqMsg :: Text -> [Pair] -> Value
encodeReqMsg = encodeMessage "request"

instance ToJSON ReqMsg where
  toJSON (EchoReqMsg msg)            = encodeReqMsg "echo" [ "message" .= msg ]
  toJSON (PuzzlesReqMsg)             = encodeReqMsg "puzzles" []
  toJSON (SignupReqMsg name email)   = encodeReqMsg "signup" [ "team-name" .= name, "email" .= email ]
  toJSON (NewReqMsg team pname diff) = encodeReqMsg "new" [ "team" .= team, "puzzle-name" .= pname, "difficulty" .= diff ]
  toJSON (ListReqMsg team)           = encodeReqMsg "list" [ "team" .= team ]
  toJSON (ShowReqMsg team puzzle)    = encodeReqMsg "show" [ "team" .= team, "puzzle" .= puzzle ]
  toJSON (SolveReqMsg team puzzle s) = encodeReqMsg "solve" [ "team" .= team, "puzzle" .= puzzle, "solution" .= s ]
  toJSON (ErrorReqMsg msg)           = encodeReqMsg "error" [ "message" .= msg ]

instance FromJSON ReqMsg where
  parseJSON (Object v) = parseR >> v .: "type" >>= parseJSON'
    where parseR :: Parser Text
          parseR = mfilter (== "request") (v .: "r")

          parseJSON' :: Text -> Parser ReqMsg
          parseJSON' "echo"    = EchoReqMsg <$> v .: "message"
          parseJSON' "puzzles" = pure PuzzlesReqMsg
          parseJSON' "signup"  = SignupReqMsg <$> v .: "team-name" <*> v .: "email"
          parseJSON' "new"     = NewReqMsg <$> v .: "team" <*> v .: "puzzle-name" <*> v .: "difficulty"
          parseJSON' "list"    = ListReqMsg <$> v .: "team"
          parseJSON' "show"    = ShowReqMsg <$> v .: "team" <*> v .: "puzzle"
          parseJSON' "solve"   = SolveReqMsg <$> v .: "team" <*> v .: "puzzle" <*> v .: "solution"
          parseJSON' "error"   = ErrorReqMsg <$> v .: "message"
          parseJSON' _         = mzero
  parseJSON _ = mzero

-- No orphan instances
arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

instance Arbitrary ReqMsg where
  arbitrary = oneof
    [ EchoReqMsg <$> arbitraryText
    , pure PuzzlesReqMsg
    , SignupReqMsg <$> arbitraryText <*> arbitraryText
    , NewReqMsg <$> arbitrary <*> arbitrary <*> arbitrary
    , ListReqMsg <$> arbitrary
    , ShowReqMsg <$> arbitrary <*> arbitrary
    , SolveReqMsg <$> arbitrary <*> arbitrary <*> arbitrary
    , ErrorReqMsg <$> arbitrary
    ]

-- | Response messages.
data ResMsg = EchoResMsg Text
            | PuzzlesResMsg [(String, Text)]      -- ^ puzzle-name, puzzle-short-description
            | SignupResMsg
            | NewResMsg Uniq Text String          -- ^ puzzle-Uniq description input
            | ListResMsg [(Uniq, String, Word16)] -- ^ puzzle-Uniq, puzzle-name, difficulty
            | ShowResMsg Uniq String Text String  -- ^ puzzle-Uniq, puzzle-name, description, input
            | SolveResMsg
            | ErrorResMsg String
  deriving (Show, Eq)

encodeResMsg :: Text -> [Pair] -> Value
encodeResMsg = encodeMessage "response"

instance ToJSON ResMsg where
  toJSON (EchoResMsg msg)                    = encodeResMsg "echo" [ "message" .= msg ]
  toJSON (PuzzlesResMsg puzzles)             = encodeResMsg "puzzles" [ "puzzles" .= puzzles ]
  toJSON (SignupResMsg)                      = encodeResMsg "signup" []
  toJSON (NewResMsg puzzle desc input)       = encodeResMsg "new" [ "puzzle" .= puzzle, "description" .= desc, "input" .= input ]
  toJSON (ListResMsg puzzles)                = encodeResMsg "list" [ "puzzles" .= puzzles ]
  toJSON (ShowResMsg puzzle name desc input) = encodeResMsg "show" [ "puzzle" .= puzzle, "puzzle-name" .= name, "description" .= desc, "input" .= input ]
  toJSON (SolveResMsg)                       = encodeResMsg "solve" []
  toJSON (ErrorResMsg msg)                   = encodeResMsg "error" [ "message" .= msg ]

instance FromJSON ResMsg where
  parseJSON (Object v) = parseR >> v .: "type" >>= parseJSON'
    where parseR :: Parser Text
          parseR = mfilter (== "response") (v .: "r")

          parseJSON' :: Text -> Parser ResMsg
          parseJSON' "echo"    = EchoResMsg <$> v .: "message"
          parseJSON' "puzzles" = PuzzlesResMsg <$> v .: "puzzles"
          parseJSON' "signup"  = pure SignupResMsg
          parseJSON' "new"     = NewResMsg <$> v .: "puzzle" <*> v .: "description" <*> v .: "input"
          parseJSON' "list"    = ListResMsg <$> v .: "puzzles"
          parseJSON' "show"    = ShowResMsg <$> v .: "puzzle" <*> v .: "puzzle-name" <*> v .: "description" <*> v .: "input"
          parseJSON' "solve"   = pure SolveResMsg
          parseJSON' "error"   = ErrorResMsg <$> v .: "message"
          parseJSON' _         = mzero
  parseJSON _ = mzero

instance Arbitrary ResMsg where
  arbitrary = oneof
    [ EchoResMsg <$> arbitraryText
    , PuzzlesResMsg <$> listOf ((,) <$> arbitrary <*> arbitraryText)
    , pure SignupResMsg
    , NewResMsg <$> arbitrary <*> arbitraryText <*> arbitrary
    , ListResMsg <$> arbitrary
    , ShowResMsg <$> arbitrary <*> arbitrary <*> arbitraryText <*> arbitrary
    , pure SolveResMsg
    , ErrorResMsg <$> arbitrary
    ]
