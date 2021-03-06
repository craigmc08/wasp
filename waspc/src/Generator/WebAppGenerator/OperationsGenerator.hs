module Generator.WebAppGenerator.OperationsGenerator
  ( genOperations,
  )
where

import Data.Aeson
  ( object,
    (.=),
  )
import Data.List (intercalate)
import Data.Maybe
  ( fromJust,
    fromMaybe,
  )
import Generator.FileDraft (FileDraft)
import qualified Generator.ServerGenerator as ServerGenerator
import qualified Generator.ServerGenerator.OperationsRoutesG as ServerOperationsRoutesG
import qualified Generator.WebAppGenerator.Common as C
import qualified Generator.WebAppGenerator.OperationsGenerator.ResourcesG as Resources
import qualified Path as P
import Wasp (Wasp)
import qualified Wasp
import qualified Wasp.Action
import qualified Wasp.Operation
import qualified Wasp.Query

genOperations :: Wasp -> [FileDraft]
genOperations wasp =
  concat
    [ genQueries wasp,
      genActions wasp,
      [C.makeSimpleTemplateFD (C.asTmplFile [P.relfile|src/operations/index.js|]) wasp],
      Resources.genResources wasp
    ]

genQueries :: Wasp -> [FileDraft]
genQueries wasp =
  concat
    [ map (genQuery wasp) (Wasp.getQueries wasp),
      [C.makeSimpleTemplateFD (C.asTmplFile [P.relfile|src/queries/index.js|]) wasp]
    ]

genActions :: Wasp -> [FileDraft]
genActions wasp =
  concat
    [ map (genAction wasp) (Wasp.getActions wasp)
    ]

genQuery :: Wasp -> Wasp.Query.Query -> FileDraft
genQuery _ query = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [P.relfile|src/queries/_query.js|]

    dstFile = C.asWebAppFile $ [P.reldir|src/queries/|] P.</> fromJust (getOperationDstFileName operation)
    tmplData =
      object
        [ "queryFnName" .= Wasp.Query._name query,
          "queryRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
    operation = Wasp.Operation.QueryOp query

genAction :: Wasp -> Wasp.Action.Action -> FileDraft
genAction _ action = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [P.relfile|src/actions/_action.js|]

    dstFile = C.asWebAppFile $ [P.reldir|src/actions/|] P.</> fromJust (getOperationDstFileName operation)
    tmplData =
      object
        [ "actionFnName" .= Wasp.Action._name action,
          "actionRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
    operation = Wasp.Operation.ActionOp action

-- | Generates string that is JS array containing names (as strings) of entities being used by given operation.
--   E.g. "['Task', 'Project']"
makeJsArrayOfEntityNames :: Wasp.Operation.Operation -> String
makeJsArrayOfEntityNames operation = "[" ++ intercalate ", " entityStrings ++ "]"
  where
    entityStrings = map (\x -> "'" ++ x ++ "'") $ fromMaybe [] $ Wasp.Operation.getEntities operation

getOperationDstFileName :: Wasp.Operation.Operation -> Maybe (P.Path P.Rel P.File)
getOperationDstFileName operation = P.parseRelFile (Wasp.Operation.getName operation ++ ".js")
