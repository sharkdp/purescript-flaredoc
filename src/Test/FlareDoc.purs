module Test.FlareDoc
  ( withPackage
  , flareDoc'
  , flareDoc
  , module FCE
  ) where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION())

import Data.Array (findIndex, index)
import Data.Either (Either(..), either, isLeft, isRight)
import Data.Either (fromLeft, fromRight)
import Data.Foldable (foldMap, fold)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Data.Tuple (Tuple(..))

import Partial.Unsafe (unsafePartial)

import Data.Argonaut (Json, toObject, class DecodeJson, decodeJson, (.?), (:=), (~>))

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser (parseMd)

import Network.HTTP.Affjax (get, AJAX)

import Signal.Channel (CHANNEL)

import DOM (DOM())

import Flare (ElementId)
import Test.FlareCheck hiding (flareDoc, flareDoc')
import Test.FlareCheck hiding (flareDoc, flareDoc') as FCE
import Test.FlareCheck as FC

comb :: forall a. Maybe a -> String -> Either String a
comb (Just x) _   = Right x
comb _        str = Left $ "Couldn't decode " <> str

infix 5 comb as ?>>=


-- | Natural transformation from `Either a` to `Maybe`.
fromEither :: forall a b. Either a b -> Maybe b
fromEither = either (const Nothing) Just

-- | A function declaration in a module.
data Declaration = Declaration String (Maybe String)

type ModuleRec =
  { name :: String
  , declarations :: Array Declaration
  }

-- | A PureScript module.
data Module = Module ModuleRec

-- | Get the name of a PureScript module.
name :: Module -> String
name (Module mod) = mod.name

-- | A PureScript package.
data Package = Package (Array Module)

instance decodePackage :: DecodeJson Package where
  decodeJson json =
    toObject json ?>>= "package" >>= \obj -> do
      modules <- M.lookup "modules" obj ?>>= "modules" >>= decodeJson
      pure (Package modules)

instance decodeModule :: DecodeJson Module where
  decodeJson json =
    toObject json ?>>= "module" >>= \obj -> do
      modName <- M.lookup "name" obj ?>>= "name" >>= decodeJson
      declarations <- M.lookup "declarations" obj ?>>= "declarations" >>= decodeJson
      pure (Module { name: modName, declarations })

instance decodeDeclaration :: DecodeJson Declaration where
  decodeJson json =
    toObject json ?>>= "declaration" >>= \obj -> do
      title <- M.lookup "title" obj ?>>= "title" >>= decodeJson
      comments <- M.lookup "comments" obj ?>>= "comments" >>= decodeJson
      pure $ Declaration title comments

-- | The full documentation of a PureScript package.
type Documentation = M.StrMap (M.StrMap (Maybe String))

-- | Parse the `psc-publish`-generated JSON format.
parseModuleJSON :: Json -> Either String Documentation
parseModuleJSON json = do
  (Package modules) <- decodeJson json
  pure $ M.fromFoldable (modTuple <$> modules)

  where
    modTuple (Module mod) = Tuple mod.name (declarationsStrMap mod.declarations)

    declarationsStrMap decls = M.fromFoldable (declTuple <$> decls)

    declTuple (Declaration title comment) = Tuple title comment

-- Very premature Markdown Render (TODO!)
render :: Partial => SlamDown -> String
render (SlamDown list) = foldMap block list
  where
    block :: forall a. Block a -> String
    block (Paragraph inl) = "<p>" <> foldMap inline inl <> "</p>"
    block (CodeBlock _ lines) = "<pre>" <> (foldMap (_ <> "\n") lines) <> "</pre>"

    inline :: forall a. Inline a -> String
    inline (Str s)      = s
    inline Space        = " "
    inline (Code _ str) = "<code>" <> str <> "</code>"
    inline SoftBreak    = " "

-- | Parse a package description and run the interactive documentation.
withPackage :: String
            -> (Documentation -> Eff (ajax :: AJAX, console :: CONSOLE, channel :: CHANNEL, dom :: DOM) Unit)
            -> Eff (ajax :: AJAX, exception :: EXCEPTION, console :: CONSOLE, channel :: CHANNEL, dom :: DOM) Unit
withPackage packageDescription run = void $ launchAff do
  r <- get packageDescription
  case parseModuleJSON r.response of
    Left err -> do
      liftEff (log err)
    Right documentation -> do
      liftEff (run documentation)

-- | Like `flareDoc`, but the HTML element can be specified.
flareDoc' :: forall t e. (Interactive t)
          => ElementId
          -> Documentation
          -> String
          -> String
          -> t
          -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
flareDoc' parentId docMap moduleName functionName x = do
  let docString = do
        decls <- M.lookup moduleName docMap
        mcomment <- M.lookup functionName decls
        comment <- mcomment
        let parsed = parseMd comment
        case parsed of
          Left error -> pure $ "Markdown parse error: " <> error
          Right md -> pure $ unsafePartial (render md)

      title = functionName
  FC.flareDoc' parentId title docString x

-- | Add an interactive documentation entry. The `String` arguments specify the
-- | module name and the function name, respectively.
flareDoc :: forall t e. (Interactive t)
         => Documentation
         -> String
         -> String
         -> t
         -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
flareDoc = flareDoc' "flaredoc"
