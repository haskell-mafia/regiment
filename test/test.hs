import           Disorder.Core.Main

import qualified Test.Regiment.Data
import qualified Test.Regiment.Vanguard.List

main :: IO ()
main =
  disorderMain [
      Test.Regiment.Data.tests
    , Test.Regiment.Vanguard.List.tests
    ]
