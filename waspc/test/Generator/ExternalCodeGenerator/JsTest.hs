module Generator.ExternalCodeGenerator.JsTest where

import Generator.ExternalCodeGenerator.Common (asGenExtFile)
import Generator.ExternalCodeGenerator.Js as Js
import qualified Path as P
import qualified StrongPath as SP
import Test.Tasty.Hspec

spec_resolveJsFileWaspImportsForExtCodeDir :: Spec
spec_resolveJsFileWaspImportsForExtCodeDir = do
  (asGenExtFile [P.relfile|extFile.js|], "import foo from 'bar'") ~> "import foo from 'bar'"
  (asGenExtFile [P.relfile|extFile.js|], "import foo from '@wasp/bar'") ~> "import foo from '../bar'"
  (asGenExtFile [P.relfile|a/extFile.js|], "import foo from  \"@wasp/bar/foo\"")
    ~> "import foo from  \"../../bar/foo\""
  where
    (path, text) ~> expectedText =
      it (SP.toFilePath path ++ " " ++ show text ++ " -> " ++ show expectedText) $ do
        Js.resolveJsFileWaspImportsForExtCodeDir (SP.fromPathRelDir [P.reldir|src|]) path text `shouldBe` expectedText
