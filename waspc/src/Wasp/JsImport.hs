module Wasp.JsImport
    ( JsImport(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

import StrongPath (Path, Rel, File)
import qualified StrongPath as SP
import ExternalCode (SourceExternalCodeDir)


-- | Represents javascript import -> "import <what> from <from>".
data JsImport = JsImport
    { _defaultImport :: !(Maybe String)
    , _namedImports :: ![String]
    -- TODO: Probably this should always be posix path, since it is JS import path.
    --   We should use Path.Posix while parsing it!? Or should we use smth else?
    , _from :: !(Path (Rel SourceExternalCodeDir) File)
    } deriving (Show, Eq)

instance ToJSON JsImport where
    toJSON jsImport = object
        [ "defaultImport" .= _defaultImport jsImport
        , "namedImports" .= _namedImports jsImport
        , "from" .= SP.toFilePath (_from jsImport)
        ]
