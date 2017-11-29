{-# LANGUAGE OverloadedStrings #-}
module ModelTests
    ( vertexFileParts
    ) where

import           Flow               ((<|))
import           Linear             (V3 (..))
import           Scene.Model.Parser (FilePart (..), parser)
import           Test.HUnit
import           Text.Megaparsec    (runParser)

-- | Test parsing of vertex 'FilePart's only.
vertexFileParts :: Assertion
vertexFileParts = do
    let parts1 = runParser parser "test" "v 1.1 2.2 3.3"
        parts2 = runParser parser "test" "v 1.1 2.2 3.3\nv 4.4 5.5 6.6"
    Right [ Vertex <| V3 1.1 2.2 3.3 ] @=? parts1
    Right [ Vertex <| V3 1.1 2.2 3.3
          , Vertex <| V3 4.4 5.5 6.6 ] @=? parts2
