{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module: Scene.Model.Parser
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- Parse a Wavefront object file. Only a subset of the file format is supported.
module Scene.Model.Parser
    ( FilePart (..)
    , Point (..)
    , splitParts
    , fromFile
    , parser
    ) where

import           Control.Applicative             (empty)
import           Control.Exception               (IOException, catch)
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Hashable
import           Data.Scientific                 (toRealFloat)
import           GHC.Generics                    (Generic)
import           Linear                          (V2 (..), V3 (..))
import           Scene                           (GLfloat)
import           Text.Megaparsec
import           Text.Megaparsec.ByteString.Lazy (Parser)
import qualified Text.Megaparsec.Lexer           as Lexer

-- | A part of the parsed model file.
data FilePart
    = Vertex !(V3 GLfloat)
    | Normal !(V3 GLfloat)
    | TexCoord !(V2 GLfloat)
    | Triangle !Point !Point !Point
    | Smooth !String
    | Mtllib !String
    | Usemtl !String
    deriving (Eq, Show)

-- | A point in a triangle face, where all the components are describing an
-- index:
-- First component is vertex index. It is mandatory.
-- Second component is texture coordinate index.
-- Third component is normal index.
data Point = Point !Int !(Maybe Int) !(Maybe Int)
    deriving (Eq, Generic, Show)

-- | Point must be hashable.
instance Hashable Point

-- | Split the file parts into: vertices, normals, texture coordinates and
-- triangles.
splitParts :: [FilePart] -> ([FilePart], [FilePart], [FilePart], [FilePart])
splitParts parts = go parts ([], [], [], [])
    where
        go :: [FilePart]
           -> ([FilePart], [FilePart], [FilePart], [FilePart])
           -> ([FilePart], [FilePart], [FilePart], [FilePart])

        -- Input list is empty. Reverse all components to get back original
        -- order.
        go [] (verts, normals, texCoords, triangles) =
            (reverse verts, reverse normals, reverse texCoords, reverse triangles)

        go (x:xs) tuple@(verts, normals, texCoords, triangles) =
            case x of
                Vertex _    -> go xs (x:verts, normals, texCoords, triangles)
                Normal _    -> go xs (verts, x:normals, texCoords, triangles)
                TexCoord _  -> go xs (verts, normals, x:texCoords, triangles)
                Triangle {} -> go xs (verts, normals, texCoords, x:triangles)
                _           -> go xs tuple

--  Read a Wavefront object from the specified file.
fromFile :: MonadIO m => FilePath -> m (Either String [FilePart])
fromFile = liftIO . fromFileUnlifted

fromFileUnlifted :: FilePath -> IO (Either String [FilePart])
fromFileUnlifted file = parseIt `catch` handler
    where
        parseIt :: IO (Either String [FilePart])
        parseIt = do
            content <- LBS.readFile file
            case runParser parser file content of
                Right parts -> return $ Right parts
                Left err    -> return $ Left (show err)

        handler :: IOException -> IO (Either String [FilePart])
        handler = return . Left . show

-- | Parse content from the 'Parser's content stream.
parser :: Parser [FilePart]
parser = manyTill (sc *> filePart) eof

filePart :: Parser FilePart
filePart = try vertex
       <|> try normal
       <|> try texCoord
       <|> try triangle
       <|> try smoothDecl
       <|> try mtllibDecl
       <|> try usemtlDecl

-- | Parse one vertex specification.
vertex :: Parser FilePart
vertex = kwV *> (Vertex <$> v3)

-- | Parse one vertex normal specification.
normal :: Parser FilePart
normal = kwVN *> (Normal <$> v3)

-- | Parse one texture coordinate specification.
texCoord :: Parser FilePart
texCoord = kwVT *> (TexCoord <$> v2)

-- | Parse one triangle face.
triangle :: Parser FilePart
triangle = kwF *> (Triangle <$> lexeme point <*> lexeme point <*> lexeme point)

-- | Smooth group. Not used.
smoothDecl :: Parser FilePart
smoothDecl = kwS *> (Smooth <$> lexeme grabString)

-- | Material declaration. Not used.
mtllibDecl :: Parser FilePart
mtllibDecl = kwMtllib *> (Mtllib <$> lexeme grabString)

-- | Material usage declaration. Not used.
usemtlDecl :: Parser FilePart
usemtlDecl = kwUsemtl *> (Usemtl <$> lexeme grabString)

-- | Parse one point.
point :: Parser Point
point = Point <$> int
              <*> (char '/' *> optional int)
              <*> (char '/' *> optional int)

-- | Parse one V2.
v2 :: Parser (V2 GLfloat)
v2 = V2 <$> lexeme glFloat <*> lexeme glFloat

-- | Parse one V3.
v3 :: Parser (V3 GLfloat)
v3 = V3 <$> lexeme glFloat <*> lexeme glFloat <*> lexeme glFloat

-- | Parse one GLfloat.
glFloat :: Parser GLfloat
glFloat = toRealFloat <$> Lexer.signed sc Lexer.scientific

-- | Parse one Int.
int :: Parser Int
int = fromIntegral <$> Lexer.signed sc Lexer.integer

-- | Keyword "v". Initiates a vertex specification.
kwV :: Parser ()
kwV = keyword "v"

-- | Keyword "vn". Initiates a vertex normal specification.
kwVN :: Parser ()
kwVN = keyword "vn"

-- | Keyword "vt". Initiates a texture coordinate specification.
kwVT :: Parser ()
kwVT = keyword "vt"

-- | Keyword "f". Initiates a face (triangle) specification.
kwF :: Parser ()
kwF = keyword "f"

-- | Keyword "s".
kwS :: Parser ()
kwS = keyword "s"

-- | Keyword "mtllib".
kwMtllib :: Parser ()
kwMtllib = keyword "mtllib"

-- | Keyword "usemtl".
kwUsemtl :: Parser ()
kwUsemtl = keyword "usemtl"

keyword :: String -> Parser ()
keyword = void . Lexer.symbol sc

-- | Space consumer; white space or comments starting with #.
sc :: Parser ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "#") empty

-- | Grab ascii until next space char.
grabString :: Parser String
grabString = manyTill asciiChar spaceChar

-- | Lexeme; a parser prepended by the space comsumer.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc
