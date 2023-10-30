{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module EncodeDocument (EncodeDocument.encode) where

-- import Data.Unique

import Data.Aeson as Aeson
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap as KM
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString as ByteString
import Data.HashMap.Strict as HMS
import Data.Kind qualified as GHC.Core.TyCo.Rep
import Data.Text as Text
import Data.Text qualified
import GHC (HsDocString, HsPatExpansion, ImportDecl, NHsValBindsLR, Name, OverLitRn, RdrName, RenamedSource, SrcSpanAnnA, Unit, WithHsDocIdentifiers)
import GHC.Core.Coercion as GHC.Core.Coercion.Axiom
import GHC.Core.Type
import GHC.Data.Bag
import GHC.Data.BooleanFormula
import GHC.Data.FastString (FastString)
import GHC.Data.Strict qualified as Ghc.Data.Strict
import GHC.Generics (Generic)
import GHC.Hs.Binds
import GHC.Hs.Doc
import GHC.Hs.Expr
import GHC.Hs.Extension (GhcPass (..), GhcRn, Pass (..))
import GHC.Hs.ImpExp
import GHC.Hs.Lit
import GHC.Hs.Pat
import GHC.Parser.Annotation
import GHC.Plugins
import GHC.Types.Avail
import GHC.Types.Basic
import GHC.Types.FieldLabel
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Tickish
import GHC.Types.Unique
import GHC.Types.Unique.DSet
import GHC.Types.Unique.Set (UniqSet, nonDetEltsUniqSet)
import GHC.Types.Var (Specificity)
import GHC.Unit.Module.Name (ModuleName (..))
import GHC.Unit.Module.Warnings
import GHC.Unit.Types
import GHC.Utils.Json
import Language.Haskell.Syntax
import Types

jsonDoc2AesonVal :: JsonDoc -> Aeson.Value
jsonDoc2AesonVal = \case
  JSNull -> Null
  JSBool b -> Bool b
  JSInt i -> Number . fromInteger . toInteger $ i
  JSString t -> String (Text.pack t)
  JSArray a -> toJSONList (jsonDoc2AesonVal <$> a)
  JSObject a -> l2o (bimap Data.Aeson.Key.fromString jsonDoc2AesonVal <$> a)

l2o :: [(Key, Value)] -> Value
l2o = Aeson.Object . KM.fromList

t2s = Aeson.String

encodeMetadata :: Metadata -> Value
encodeMetadata mi = l2o [("Format version", toJSON $ formatVersion mi), ("GHC version", toJSON $ ghcVersion mi), ("Haddock version", toJSON $ haddockVersion mi)]

encode :: Metadata -> RenamedSource -> Value
encode md rs = l2o [("Metadata", encodeMetadata md), ("Data", encodeRenamedSource rs)]

encodeRenamedSource :: RenamedSource -> Value
encodeRenamedSource (a, b, c, d) = toJSONList [toJSON "Renamed Source", toJSON a, toJSON b, toJSON c, toJSON d]

deriving instance Generic (HsExpr GhcRn)

instance ToJSON (HsExpr GhcRn)

deriving instance Generic (HsExpansion a b)

instance (ToJSON a, ToJSON b) => ToJSON (HsExpansion a b)

deriving instance Generic (GenLocated a b)

instance (ToJSON a, ToJSON b) => ToJSON (GenLocated a b)

deriving instance Generic (HsWildCardBndrs a b)

instance (ToJSON b) => ToJSON (HsWildCardBndrs GhcRn b)

deriving instance Generic (ParStmtBlock a b)

instance ToJSON (ParStmtBlock GhcRn GhcRn)

deriving instance Generic (HsFieldBind a b)

instance (ToJSON a, ToJSON b) => ToJSON (HsFieldBind a b)

deriving instance Generic (HsRecFields a b)

instance (ToJSON b) => ToJSON (HsRecFields GhcRn b)

deriving instance Generic (HsLocalBindsLR a b)

instance ToJSON (HsLocalBindsLR GhcRn GhcRn)

deriving instance Generic (GRHS a b)

instance (ToJSON a) => ToJSON (GRHS GhcRn a)

deriving instance Generic (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))

instance ToJSON (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))

deriving instance Generic (HsPragE a)

instance ToJSON (HsPragE GhcRn)

deriving instance Generic (HsCmdTop a)

instance ToJSON (HsCmdTop GhcRn)

deriving instance Generic (Pat a)

instance ToJSON (Pat GhcRn)

deriving instance Generic (HsSplice a)

instance ToJSON (HsSplice GhcRn)

deriving instance Generic (HsQuote a)

instance ToJSON (HsQuote GhcRn)

deriving instance Generic (ArithSeqInfo a)

instance ToJSON (ArithSeqInfo GhcRn)

deriving instance Generic (SrcSpanAnn' a)

instance (ToJSON a) => ToJSON (SrcSpanAnn' a)

deriving instance Generic (HsSigType a)

instance ToJSON (HsSigType GhcRn)

deriving instance Generic (DotFieldOcc a)

instance ToJSON (DotFieldOcc GhcRn)

deriving instance Generic (EpAnn a)

instance (ToJSON a) => ToJSON (EpAnn a)

deriving instance Generic DataConCantHappen

instance ToJSON DataConCantHappen

deriving instance Generic (FieldLabelStrings a)

instance ToJSON (FieldLabelStrings GhcRn)

deriving instance Generic (ApplicativeArg a)

instance ToJSON (ApplicativeArg GhcRn)

deriving instance Generic SyntaxExprRn

instance ToJSON SyntaxExprRn

deriving instance Generic AddEpAnn

instance ToJSON AddEpAnn

deriving instance Generic (FieldOcc a)

instance ToJSON (FieldOcc GhcRn)

deriving instance Generic (AmbiguousFieldOcc a)

instance ToJSON (AmbiguousFieldOcc GhcRn)

deriving instance Generic (HsToken tok)

instance ToJSON (HsToken tok)

deriving instance Generic (HsTupArg a)

instance ToJSON (HsTupArg GhcRn)

deriving instance Generic AnnList

instance ToJSON AnnList

deriving instance Generic AnnListItem

instance ToJSON AnnListItem

deriving instance Generic SrcSpan

instance ToJSON SrcSpan

deriving instance Generic (HsValBindsLR a b)

instance ToJSON (HsValBindsLR GhcRn GhcRn)

deriving instance Generic (HsType GhcRn)

deriving instance Generic TransForm

instance ToJSON TransForm

deriving instance Generic NoExtField

instance ToJSON NoExtField

deriving instance Generic XBindStmtRn

instance ToJSON XBindStmtRn

deriving instance Generic NoEpAnns

instance ToJSON NoEpAnns

deriving instance Generic (HsIPBinds GhcRn)

instance ToJSON (HsIPBinds GhcRn)

deriving instance Generic GrhsAnn

instance ToJSON GrhsAnn

deriving instance Generic (Match GhcRn a)

instance (ToJSON a, ToJSON (Anno (GRHS GhcRn a))) => ToJSON (Match GhcRn a)

deriving instance Generic StringLiteral

instance ToJSON StringLiteral

deriving instance Generic (HsCmd GhcRn)

instance ToJSON (HsCmd GhcRn)

deriving instance Generic (HsOverLit GhcRn)

instance ToJSON (HsOverLit GhcRn)

-- TODO check if relying on constructors directly is okay
deriving instance Generic SourceText

instance ToJSON SourceText

deriving instance Generic (HsConDetails ta r a)

instance (ToJSON ta, ToJSON r, ToJSON a) => ToJSON (HsConDetails ta r a)

deriving instance Generic (HsSplicedThing GhcRn)

instance ToJSON (HsSplicedThing GhcRn)

deriving instance Generic (HsGroup GhcRn)

instance ToJSON (HsGroup GhcRn)

deriving instance Generic (HsOuterTyVarBndrs Specificity GhcRn)

instance ToJSON (HsOuterTyVarBndrs Specificity GhcRn)

deriving instance Generic AnnFieldLabel

instance ToJSON AnnFieldLabel

deriving instance Generic Anchor

instance ToJSON Anchor

deriving instance Generic HsDoFlavour

instance ToJSON HsDoFlavour

deriving instance Generic AnnKeywordId

instance ToJSON AnnKeywordId

deriving instance Generic NameAnn

instance ToJSON NameAnn

-- TODO check if relying on constructors directly is okay
deriving instance Generic RdrName

instance ToJSON RdrName

deriving instance Generic TrailingAnn

instance ToJSON TrailingAnn

deriving instance Generic (Ghc.Data.Strict.Maybe BufSpan)

instance ToJSON (Ghc.Data.Strict.Maybe BufSpan)

deriving instance Generic PendingRnSplice

instance ToJSON PendingRnSplice

deriving instance Generic Origin

instance ToJSON Origin

deriving instance Generic AnnPragma

instance ToJSON AnnPragma

deriving instance Generic (HsPatExpansion (Pat GhcRn) (Pat GhcRn))

instance ToJSON (HsPatExpansion (Pat GhcRn) (Pat GhcRn))

deriving instance Generic GHC.Types.Basic.Boxity

instance ToJSON GHC.Types.Basic.Boxity

deriving instance Generic TokenLocation

instance ToJSON TokenLocation

deriving instance Generic (HsPatSigType GhcRn)

instance ToJSON (HsPatSigType GhcRn)

deriving instance Generic SpliceDecoration

instance ToJSON SpliceDecoration

deriving instance Generic (HsDecl GhcRn)

instance ToJSON (HsDecl GhcRn)

instance ToJSON FastString where
  toJSON :: FastString -> Value
  toJSON = String . Data.Text.pack . show

deriving instance Generic GHC.Types.Fixity.Fixity

instance ToJSON GHC.Types.Fixity.Fixity

deriving instance Generic EpAnnComments

instance ToJSON EpAnnComments

deriving instance Generic EpaLocation

instance ToJSON EpaLocation

deriving instance Generic UnhelpfulSpanReason

instance ToJSON UnhelpfulSpanReason

deriving instance Generic LamCaseVariant

instance ToJSON LamCaseVariant

deriving instance Generic HsIPName

instance ToJSON HsIPName

instance ToJSON RealSrcSpan where
  toJSON = jsonDoc2AesonVal . GHC.Utils.Json.json

deriving instance Generic (NHsValBindsLR GhcRn)

instance ToJSON (NHsValBindsLR GhcRn)

deriving instance Generic (Sig GhcRn)

instance ToJSON (Sig GhcRn)

deriving instance Generic (HsForAllTelescope GhcRn)

instance ToJSON (HsForAllTelescope GhcRn)

deriving instance Generic (HsBindLR GhcRn GhcRn)

instance ToJSON (HsBindLR GhcRn GhcRn)

deriving instance Generic (HsArrow GhcRn)

instance ToJSON (HsArrow GhcRn)

instance ToJSON ByteString where
  toJSON :: ByteString -> Value
  toJSON = toJSON . ByteString.unpack

deriving instance Generic AnnSortKey

instance ToJSON AnnSortKey

deriving instance Generic (ConDeclField GhcRn)

instance ToJSON (ConDeclField GhcRn)

deriving instance Generic AnnContext

instance ToJSON AnnContext

deriving instance Generic IntegralLit

instance ToJSON IntegralLit

deriving instance Generic (IPBind GhcRn)

instance ToJSON (IPBind GhcRn)

deriving instance Generic (GRHSs GhcRn a)

instance (ToJSON a, ToJSON (Anno (GRHS GhcRn a))) => ToJSON (GRHSs GhcRn a)

deriving instance Generic (StmtLR GhcRn GhcRn c)

instance (ToJSON (Anno (StmtLR GhcRn GhcRn a)), ToJSON (Anno [GenLocated (Anno (StmtLR GhcRn GhcRn a)) (StmtLR GhcRn GhcRn a)]), ToJSON a) => ToJSON (StmtLR GhcRn GhcRn a)

deriving instance Generic PromotionFlag

instance ToJSON PromotionFlag

deriving instance Generic AnnParen

instance ToJSON AnnParen

deriving instance Generic (HsMatchContext GhcRn)

instance ToJSON (HsMatchContext GhcRn)

deriving instance Generic HsTupleSort

instance ToJSON HsTupleSort

deriving instance Generic HsSrcBang

instance ToJSON HsSrcBang

deriving instance Generic HsTyLit

instance ToJSON HsTyLit

deriving instance Generic FractionalLit

instance ToJSON FractionalLit

deriving instance Generic (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))

instance ToJSON (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))

deriving instance Generic LexicalFixity

instance ToJSON LexicalFixity

deriving instance Generic OverLitRn

instance ToJSON OverLitRn

deriving instance Generic HsArrAppType

instance ToJSON HsArrAppType

deriving instance Generic OverLitVal

instance ToJSON OverLitVal

deriving instance Generic (SpliceDecl GhcRn)

instance ToJSON (SpliceDecl GhcRn)

deriving instance Generic AnchorOperation

instance ToJSON AnchorOperation

-- TODO fix this instance
instance ToJSON ModuleName where
  toJSON mn = toJSON (show mn)

deriving instance Generic NameAdornment

instance ToJSON NameAdornment

deriving instance Generic BufSpan

instance ToJSON BufSpan

deriving instance Generic (TyClGroup GhcRn)

instance ToJSON (TyClGroup GhcRn)

deriving instance Generic (DerivDecl GhcRn)

instance ToJSON (DerivDecl GhcRn)

deriving instance Generic (GenModule a)

instance (ToJSON a) => ToJSON (GenModule a)

deriving instance Generic (FixitySig GhcRn)

instance ToJSON (FixitySig GhcRn)

deriving instance Generic (HsTyVarBndr a GhcRn)

instance (ToJSON a) => ToJSON (HsTyVarBndr a GhcRn)

deriving instance Generic UntypedSpliceFlavour

instance ToJSON UntypedSpliceFlavour

deriving instance Generic HsPSRn

instance ToJSON HsPSRn

deriving instance Generic (DefaultDecl GhcRn)

instance ToJSON (DefaultDecl GhcRn)

deriving instance Generic Specificity

instance ToJSON Specificity

deriving instance Generic (ForeignDecl GhcRn)

instance ToJSON (ForeignDecl GhcRn)

deriving instance Generic FixityDirection

instance ToJSON FixityDirection

deriving instance Generic EpaComment

instance ToJSON EpaComment

deriving instance Generic RecFlag

instance ToJSON RecFlag

deriving instance Generic (WarnDecls GhcRn)

instance ToJSON (WarnDecls GhcRn)

deriving instance Generic (RoleAnnotDecl GhcRn)

instance ToJSON (RoleAnnotDecl GhcRn)

deriving instance Generic DeltaPos

instance ToJSON DeltaPos

deriving instance Generic HsDocString

instance ToJSON HsDocString

deriving instance Generic (AnnDecl GhcRn)

instance ToJSON (AnnDecl GhcRn)

deriving instance Generic (DocDecl GhcRn)

instance ToJSON (DocDecl GhcRn)

deriving instance Generic (BooleanFormula a)

instance (ToJSON a) => ToJSON (BooleanFormula a)

deriving instance Generic (RuleDecls GhcRn)

instance ToJSON (RuleDecls GhcRn)

deriving instance Generic InlinePragma

instance ToJSON InlinePragma

deriving instance Generic (StandaloneKindSig GhcRn)

instance ToJSON (StandaloneKindSig GhcRn)

deriving instance Generic (PatSynBind GhcRn GhcRn)

instance ToJSON (PatSynBind GhcRn GhcRn)

deriving instance Generic (HsUniToken "->" "\8594")

instance ToJSON (HsUniToken "->" "\8594")

deriving instance Generic (InstDecl GhcRn)

instance ToJSON (InstDecl GhcRn)

deriving instance Generic AnnSig

instance ToJSON AnnSig

deriving instance Generic (HsLinearArrowTokens GhcRn)

instance ToJSON (HsLinearArrowTokens GhcRn)

deriving instance Generic IsUnicodeSyntax

instance ToJSON IsUnicodeSyntax

deriving instance Generic ParenType

instance ToJSON ParenType

deriving instance Generic (HsStmtContext GhcRn)

instance ToJSON (HsStmtContext GhcRn)

deriving instance Generic SrcUnpackedness

instance ToJSON SrcUnpackedness

deriving instance Generic FractionalExponentBase

instance ToJSON FractionalExponentBase

deriving instance Generic SpliceExplicitFlag

instance ToJSON SpliceExplicitFlag

deriving instance Generic (GenInstantiatedUnit a)

instance (ToJSON a) => ToJSON (GenInstantiatedUnit a)

deriving instance Generic (TyClDecl GhcRn)

instance ToJSON (TyClDecl GhcRn)

deriving instance Generic SrcStrictness

instance ToJSON SrcStrictness

deriving instance Generic (Definite a)

instance (ToJSON a) => ToJSON (Definite a)

deriving instance Generic BufPos

instance ToJSON BufPos

deriving instance Generic HsArrowMatchContext

instance ToJSON HsArrowMatchContext

deriving instance Generic UnitId

instance ToJSON UnitId

deriving instance Generic EpaCommentTok

instance ToJSON EpaCommentTok

deriving instance Generic (WarnDecl GhcRn)

instance ToJSON (WarnDecl GhcRn)

deriving instance Generic HsDocStringChunk

instance ToJSON HsDocStringChunk

deriving instance Generic (AnnProvenance GhcRn)

instance ToJSON (AnnProvenance GhcRn)

deriving instance Generic (RuleDecl GhcRn)

instance ToJSON (RuleDecl GhcRn)

deriving instance Generic RuleMatchInfo

instance ToJSON RuleMatchInfo

deriving instance Generic (RecordPatSynField GhcRn)

instance ToJSON (RecordPatSynField GhcRn)

deriving instance Generic (ClsInstDecl GhcRn)

instance ToJSON (ClsInstDecl GhcRn)

deriving instance Generic (DerivStrategy GhcRn)

instance ToJSON (DerivStrategy GhcRn)

deriving instance Generic ForeignExport

instance ToJSON ForeignExport

deriving instance Generic Activation

instance ToJSON Activation

deriving instance Generic (HsPatSynDir GhcRn)

instance ToJSON (HsPatSynDir GhcRn)

deriving instance Generic (DataFamInstDecl GhcRn)

instance ToJSON (DataFamInstDecl GhcRn)

deriving instance Generic (GenUnit a)

instance (ToJSON a) => ToJSON (GenUnit a)

deriving instance Generic (TyFamInstDecl GhcRn)

instance ToJSON (TyFamInstDecl GhcRn)

deriving instance Generic (RuleBndr GhcRn)

instance ToJSON (RuleBndr GhcRn)

deriving instance Generic OverlapMode

instance ToJSON OverlapMode

deriving instance Generic HsRuleRn

instance ToJSON HsRuleRn

deriving instance Generic (FunDep GhcRn)

instance ToJSON (FunDep GhcRn)

deriving instance Generic ForeignImport

instance ToJSON ForeignImport

deriving instance Generic InlineSpec

instance ToJSON InlineSpec

deriving instance Generic (HsDataDefn GhcRn)

instance ToJSON (HsDataDefn GhcRn)

deriving instance Generic (ConDecl GhcRn)

instance ToJSON (ConDecl GhcRn)

deriving instance Generic CCallConv

instance ToJSON CCallConv

deriving instance Generic Safety

instance ToJSON Safety

deriving instance Generic (HsDerivingClause GhcRn)

instance ToJSON (HsDerivingClause GhcRn)

deriving instance Generic CExportSpec

instance ToJSON CExportSpec

deriving instance Generic (LHsQTyVars GhcRn)

instance ToJSON (LHsQTyVars GhcRn)

deriving instance Generic (DerivClauseTys GhcRn)

instance ToJSON (DerivClauseTys GhcRn)

deriving instance Generic Header

instance ToJSON Header

deriving instance Generic NewOrData

instance ToJSON NewOrData

deriving instance Generic CImportSpec

instance ToJSON CImportSpec

deriving instance Generic (FamilyDecl GhcRn)

instance ToJSON (FamilyDecl GhcRn)

deriving instance Generic HsDocStringDecorator

instance ToJSON HsDocStringDecorator

instance ToJSON GHC.Types.Unique.Unique where
  toJSON a = toJSON $ getKey a

deriving instance Generic (FamilyInfo GhcRn)

instance ToJSON (FamilyInfo GhcRn)

deriving instance Generic CCallTarget

instance ToJSON CCallTarget

deriving instance Generic CType

instance ToJSON CType

deriving instance Generic DataDeclRn

instance ToJSON DataDeclRn

deriving instance Generic (FamilyResultSig GhcRn)

instance ToJSON (FamilyResultSig GhcRn)

deriving instance Generic (InjectivityAnn GhcRn)

instance ToJSON (InjectivityAnn GhcRn)

deriving instance Generic (FamEqn GhcRn a)

instance (ToJSON a) => ToJSON (FamEqn GhcRn a)

deriving instance Generic GHC.Core.Coercion.Axiom.Role

instance ToJSON Role

deriving instance Generic TopLevelFlag

instance ToJSON TopLevelFlag

deriving instance Generic (HsOuterTyVarBndrs () GhcRn)

instance ToJSON (HsOuterTyVarBndrs () GhcRn)

deriving instance Generic (HsArg a b)

instance (ToJSON a, ToJSON b) => ToJSON (HsArg a b)

deriving instance Generic (WarningTxt GhcRn)

instance ToJSON (WarningTxt GhcRn)

deriving instance Generic (WithHsDocIdentifiers a GhcRn)

instance (ToJSON a) => ToJSON (WithHsDocIdentifiers a GhcRn)

deriving instance Generic (HsScaled GhcRn a)

instance (ToJSON a) => ToJSON (HsScaled GhcRn a)

deriving instance Generic (HsConDeclGADTDetails GhcRn)

instance ToJSON (HsConDeclGADTDetails GhcRn)

deriving instance Generic (ImportDecl GhcRn)

instance ToJSON (ImportDecl GhcRn)

deriving instance Generic (IEWrappedName a)

instance (ToJSON a) => ToJSON (IEWrappedName a)

deriving instance Generic (IE GhcRn)

instance ToJSON (IE GhcRn)

deriving instance Generic PkgQual

instance ToJSON PkgQual

deriving instance Generic IsBootInterface

instance ToJSON IsBootInterface

deriving instance Generic ImportDeclQualifiedStyle

instance ToJSON ImportDeclQualifiedStyle

deriving instance Generic FieldLabel

instance ToJSON FieldLabel

deriving instance Generic IEWildcard

instance ToJSON IEWildcard

deriving instance Generic FieldSelectors

instance ToJSON FieldSelectors

deriving instance Generic DuplicateRecordFields

instance ToJSON DuplicateRecordFields

deriving instance Generic AvailInfo

instance ToJSON AvailInfo

deriving instance Generic GreName

instance ToJSON GreName

-- ToJSON (HsLit GhcRn) must be manually generated because there is no way to implement toJSON Type-
instance ToJSON (HsLit GhcRn) where
  toJSON :: HsLit GhcRn -> Value
  toJSON x = toJSONList $ case x of
    HsChar a b -> [toJSON "HsChar", toJSON a, toJSON b]
    HsCharPrim a b -> [toJSON "HsCharPrim", toJSON a, toJSON b]
    HsString a b -> [toJSON "HsString", toJSON a, toJSON b]
    HsStringPrim a b -> [toJSON "HsStringPrim", toJSON a, toJSON b]
    HsInt a b -> [toJSON "HsInt", toJSON a, toJSON b]
    HsIntPrim a b -> [toJSON "HsIntPrim", toJSON a, toJSON b]
    HsWordPrim a b -> [toJSON "HsWordPrim", toJSON a, toJSON b]
    HsInteger a b _ -> [toJSON "HsInteger", toJSON a, toJSON b]
    HsRat a b _ -> [toJSON "HsRat", toJSON a, toJSON b]
    HsFloatPrim a b -> [toJSON "HsFloatPrim", toJSON a, toJSON b]
    HsDoublePrim a b -> [toJSON "HsDoublePrim", toJSON a, toJSON b]

-- XLit a -> dataConCantHappen a

-- ToJSON (HsType GhcRn) must be manually generated because there is no way to implement toJSON Type
instance ToJSON (HsType GhcRn) where
  toJSON x = toJSONList $ case x of
    HsForAllTy a b c -> [toJSON "HsForallTy", toJSON a, toJSON b, toJSON c]
    HsQualTy a b c -> [toJSON "HsQualTy", toJSON a, toJSON b, toJSON c]
    HsTyVar a b c -> [toJSON "HsTyVar", toJSON a, toJSON b, toJSON c]
    HsAppTy a b c -> [toJSON "HsAppTy", toJSON a, toJSON b, toJSON c]
    HsAppKindTy a b c -> [toJSON "HsAppKindTy", toJSON a, toJSON b, toJSON c]
    HsFunTy a b c d -> [toJSON "HsFunTy", toJSON a, toJSON b, toJSON c, toJSON d]
    HsListTy a b -> [toJSON "HsListTy", toJSON a, toJSON b]
    HsTupleTy a b c -> [toJSON "HsTupleTy", toJSON a, toJSON b, toJSON c]
    HsSumTy a b -> [toJSON "HsSumTy", toJSON a, toJSON b]
    HsOpTy a b c d e -> [toJSON "HsOpTy", toJSON a, toJSON b, toJSON c, toJSON d, toJSON e]
    HsParTy a b -> [toJSON "HsParTy", toJSON a, toJSON b]
    HsIParamTy a b c -> [toJSON "HsIPAramTy", toJSON a, toJSON b, toJSON c]
    HsStarTy a b -> [toJSON "HsStarTy", toJSON a, toJSON b]
    HsKindSig a b c -> [toJSON "HsKindSig", toJSON a, toJSON b, toJSON c]
    HsSpliceTy a b -> [toJSON "HsSpliceTy", toJSON a, toJSON b]
    HsDocTy a b c -> [toJSON "HsDocTy", toJSON a, toJSON b, toJSON c]
    HsBangTy a b c -> [toJSON "HsBangTy", toJSON a, toJSON b, toJSON c]
    HsRecTy a b -> [toJSON "HsRecTy", toJSON a, toJSON b]
    HsExplicitListTy a b c -> [toJSON "HsExplicitListTy", toJSON a, toJSON b, toJSON c]
    HsExplicitTupleTy a b -> [toJSON "HsExplicitTupleTy", toJSON a, toJSON b]
    HsTyLit a b -> [toJSON "HstyLit", toJSON a, toJSON b]
    HsWildCardTy a -> [toJSON "HsWildCardTy", toJSON a]
    XHsType a -> [toJSON "HsCoreTy"]

-- output a bag to JSON by converting it to a list of type [a], NOT of type [(Int, a)]
instance (ToJSON a) => ToJSON (Bag a) where
  toJSON :: (ToJSON a) => Bag a -> Value
  toJSON = toJSON . bagToList

-- output a UniqSet by converting it to a list first
instance (ToJSON a) => ToJSON (UniqSet a) where
  toJSON :: (ToJSON a) => UniqSet a -> Value
  toJSON x = toJSON $ nonDetEltsUniqSet x

-- output a UniqDSet by converting it to a list first
instance (ToJSON a) => ToJSON (UniqDSet a) where
  toJSON :: (ToJSON a) => UniqDSet a -> Value
  toJSON x = toJSON (uniqDSetToList x)

-- TODO: fix this instance
instance ToJSON Name where
  toJSON :: Name -> Value
  toJSON name = toJSONList [toJSON "Name", toJSON $ nameOccName name, toJSON $ nameUnique name, toJSON $ nameSrcSpan name]

-- There isn't a good way to serialize ThModFinalizers
instance ToJSON ThModFinalizers where
  toJSON :: ThModFinalizers -> Value
  toJSON _ = "<ThModFinalizers>"

-- TODO fix this constructor
instance ToJSON Var where
  toJSON :: Var -> Value
  toJSON _ = "<Var>"

-- This is a placeholder instance because GenTickish likely isn't needed in the deserialized result
instance ToJSON (GenTickish a) where
  toJSON :: GenTickish a -> Value
  toJSON _ = String "<GenTickish>"

instance ToJSON OccName where
  toJSON occname = String $ Text.pack $ unpackFS $ occNameFS occname
