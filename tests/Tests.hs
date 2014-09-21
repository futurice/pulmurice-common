import Data.Aeson
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Pulmurice.Common.Message

main :: IO ()
main = defaultMain qcProps

requestMessageProp :: ReqMsg -> Property
requestMessageProp msg = Just msg === decode (encode msg)

responseMessageProp :: ResMsg -> Property
responseMessageProp msg = Just msg === decode (encode msg)

qcProps :: TestTree
qcProps = testGroup "QuickCheck properties"
  [ QC.testProperty "ReqMsg: decode . encode = id" requestMessageProp
  , QC.testProperty "ResMsg: decode . encode = id" responseMessageProp
  ]
