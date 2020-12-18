module Psl.Model.Schema
    ( astToSchema
    ) where

import PslModelAst


astToSchema :: Model -> String
astToSchema = error "TODO"



-- TODO: I should make sure to skip attributes that are not known in prisma.
--   Or maybe it would be better if that was done in previous step, where
--   we basically edit the AST by kicking out those attributes.


-- TODO: Rename PslModelAst module to Psl.Model.Ast .
