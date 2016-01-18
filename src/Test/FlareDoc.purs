module Test.FlareDoc
  ( parseModuleJSON
  , document'
  , document
  , module Test.FlareCheck
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Array (findIndex, index)
import Data.Either (Either(..), either, isLeft, isRight)
import Data.Either.Unsafe (fromLeft, fromRight)
import Data.Foldable (foldMap, fold)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Data.Tuple (Tuple(..))

import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Decode

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser
import Text.Markdown.SlamDown.Pretty

import Signal.Channel (Chan())

import DOM (DOM())

import Test.FlareCheck
import Test.FlareCheck as FC

fromEither :: forall a b. Either a b -> Maybe b
fromEither = either (const Nothing) Just

data Declaration = Declaration String (Maybe String)

type ModuleRec =
  { name :: String
  , declarations :: Array Declaration
  }

data Module = Module ModuleRec

name :: Module -> String
name (Module mod) = mod.name

data Package = Package (Array Module)

instance decodePackage :: DecodeJson Package where
  decodeJson json =
    toObject json ?>>= "package" >>= \obj -> do
      modules <- M.lookup "modules" obj ?>>= "modules" >>= decodeJson
      return (Package modules)

instance decodeModule :: DecodeJson Module where
  decodeJson json =
    toObject json ?>>= "module" >>= \obj -> do
      modName <- M.lookup "name" obj ?>>= "name" >>= decodeJson
      declarations <- M.lookup "declarations" obj ?>>= "declarations" >>= decodeJson
      return (Module { name: modName, declarations })

instance decodeDeclaration :: DecodeJson Declaration where
  decodeJson json =
    toObject json ?>>= "declaration" >>= \obj -> do
      title <- M.lookup "title" obj ?>>= "title" >>= decodeJson
      comments <- M.lookup "comments" obj ?>>= "comments" >>= decodeJson
      return $ Declaration title comments

type Documentation = M.StrMap (M.StrMap (Maybe String))

parseModuleJSON :: Json -> Either String Documentation
parseModuleJSON json = do
  (Package modules) <- decodeJson json
  return $ M.fromFoldable (modTuple <$> modules)

  where
    modTuple (Module mod) = Tuple mod.name (declarationsStrMap mod.declarations)

    declarationsStrMap decls = M.fromFoldable (declTuple <$> decls)

    declTuple (Declaration title comment) = Tuple title comment

-- Premature Markdown Render (TODO!)
render :: SlamDown -> String
render (SlamDown list) = foldMap block list
  where
    block :: Block -> String
    block (Paragraph inl) = "<p>" <> foldMap inline inl <> "</p>"
    block (CodeBlock _ lines) = "<pre>" <> (foldMap (<> "\n") lines) <> "</pre>"

    inline :: Inline -> String
    inline (Str s) = s
    inline Space = " "
    inline (Code _ str) = "<code>" <> str <> "</code>"
    inline SoftBreak = " "

document' :: forall t e. (Interactive t)
          => ElementId
          -> Documentation
          -> String
          -> String
          -> t
          -> Eff (chan :: Chan, dom :: DOM | e) Unit
document' parentId docMap moduleName functionName x = do
  let docString = do
        decls <- M.lookup moduleName docMap
        mcomment <- M.lookup functionName decls
        comment <- mcomment
        let parsed = (render <<< parseMd) comment
        return parsed

      title = moduleName ++ "." ++ functionName
  FC.flareDoc' parentId title docString x

document :: forall t e. (Interactive t)
         => Documentation
         -> String
         -> String
         -> t
         -> Eff (chan :: Chan, dom :: DOM | e) Unit
document = document' "flaredoc"
