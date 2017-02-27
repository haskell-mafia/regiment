import           Disorder.Core.Main

import qualified Test.IO.Regiment.IO

main :: IO ()
main =
  disorderMain [
      Test.IO.Regiment.IO.tests
    ]
