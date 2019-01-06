{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Data.Struere.Editor.Mode.Haskell where

import qualified Data.Text                        as T
import           GHC.Generics
import           Prelude                          hiding (pure, (*>), (<$>),
                                                   (<*), (<*>), (<|>))

import           Language.Haskell.Exts.Comments
import           Language.Haskell.Exts.ExactPrint
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
-- import           Language.Haskell.Exts.SimpleComments

import           Data.Struere.Parser
import           Data.Struere.Structural
-- import           Data.Struere.Scaffold
-- import           Data.Struere.Viewer
import           Data.Struere.Editor.Editable
import           Data.Struere.Isomorphism

import           Debug.Trace


defineIsomorphisms ''Decl
defineIsomorphisms ''Match
defineIsomorphisms ''Name
defineIsomorphisms ''Rhs
var = $(constructorIso 'Var)
defineIsomorphisms ''QName


type HaskellFile = Module Info
type Info = (SrcSpanInfo, [Comment])

type Test = Exp Info

test :: Structural f => f Test
test = expr
test1 :: Test
Right test1 = traceShowId $ runParser test $ T.pack "abcd"

instance Editable (Module (SrcSpanInfo, [Comment])) where
    structure = modulestr

    parser s = case parseModuleWithComments defaultParseMode $ T.unpack s of
        ParseOk (mod, comments) -> Right $ associateHaddock (mod, comments)
        ParseFailed loc s       -> Left s

    printer mod =
        T.pack . exactPrint (fmap fst mod) $ snd (ann mod)

modulestr :: Structural f => f (Module (SrcSpanInfo, [Comment]))
modulestr = moduleiso <$> many decl

moduleiso :: Iso [Decl (SrcSpanInfo, [Comment])]
                 (Module (SrcSpanInfo, [Comment]))
moduleiso = isomorphism to from
  where
    to decls = Just $ Module (noSrcSpan, []) Nothing [] [] decls
    from (Module _ _ _ _ decls) = Just decls
    from _                      = Nothing

noInfo :: Structural f => f Info
noInfo = pure (noSrcSpan, [])

decl :: Structural f => f (Decl Info)
decl = funBind <$> noInfo <*> many1 matchc

matchc :: Structural f => f (Match Info)
matchc = match <$> noInfo <*> empty <*> pure [] <*> rhs <*> pure Nothing

rhs :: Structural f => f (Rhs Info)
rhs = unGuardedRhs <$> noInfo <*> expr


expr, fexpr, aexpr :: Structural f => f (Exp Info)
expr = fexpr
fexpr = aexpr
aexpr = var <$> noInfo <*> qname

qname :: Structural f => f (QName Info)
qname = unQual <$> noInfo <*> name

name :: Structural f => f (Name Info)
name = ident <$> noInfo <*> identifier

identifier :: Structural f => f String
identifier = sub defaultLevel $ cons <$> letter <*> many alphaNum


name1 :: Name Info
name1 = Ident (noSrcSpan, []) "abcdef"
