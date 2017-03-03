import           Disorder.Core.Main

import qualified Test.Regiment.Data

main :: IO ()
main =
  disorderMain [
      Test.Regiment.Data.tests
    ]
