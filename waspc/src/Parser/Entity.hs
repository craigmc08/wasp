module Parser.Entity
    ( entity
    ) where

import Text.Parsec.String (Parser)

import qualified Wasp.Entity as E
import qualified Lexer as L
import qualified Parser.PslModel
import qualified PslModelAst as Model

entity :: Parser E.Entity
entity = do
    _ <- L.reserved L.reservedNameEntity
    name <- L.identifier
    _ <- L.symbol "{=psl"
    pslModelBody <- Parser.PslModel.body
    _ <- L.symbol "psl=}"

    return E.Entity
        { E._name = name
        , E._fields = getEntityFields pslModelBody
        , E._pslModelBody = pslModelBody
        }

getEntityFields :: Model.Body -> [E.Field]
getEntityFields (Model.Body pslElements) = map pslFieldToEntityField pslFields
  where
    pslFields = [field | (Model.ElementField field) <- pslElements]

    pslFieldToEntityField :: Model.Field -> E.Field
    pslFieldToEntityField pslField = E.Field
        { E._fieldName = Model._name pslField
        , E._fieldType = pslFieldTypeToEntityFieldType
                    (Model._type pslField)
                    (Model._typeModifiers pslField)
        }

    pslFieldTypeToEntityFieldType
        :: Model.FieldType
        -> [Model.FieldTypeModifier]
        -> E.FieldType
    pslFieldTypeToEntityFieldType fType fTypeModifiers =
        let scalar = pslFieldTypeToScalar fType
        in case fTypeModifiers of
               [] -> E.FieldTypeScalar scalar
               [Model.List] -> E.FieldTypeComposite $ E.List scalar
               [Model.Optional] -> E.FieldTypeComposite $ E.Optional scalar
               _ -> error "Not a valid list of modifiers."

    pslFieldTypeToScalar :: Model.FieldType -> E.Scalar
    pslFieldTypeToScalar fType = case fType of
        Model.String -> E.String
        Model.Boolean -> E.Boolean
        Model.Int -> E.Int
        Model.Float -> E.Float
        Model.DateTime -> E.DateTime
        Model.Json -> E.Json
        Model.UserType name -> E.UserType name
