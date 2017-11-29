module Main
    ( main
    ) where


import           ModelTests                     (vertexFileParts)
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "Model tests - Wavefront file parsing"
        [ testCase "Parse vertex FileParts" vertexFileParts
        ]
    ]
