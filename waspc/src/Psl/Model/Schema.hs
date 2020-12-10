module Psl.Model.Schema
    ( astToSchema
    ) where

import PslModelAst


astToSchema :: Model -> String
astToSchema = error "TODO"

-- TODO: I should make sure to skip attributes that are not known in prisma.


-- TODO: Rename PslModelAst module to Psl.Model.Ast .
