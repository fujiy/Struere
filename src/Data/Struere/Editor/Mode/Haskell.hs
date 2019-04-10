{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Data.Struere.Editor.Mode.Haskell where

import qualified Data.Text                        as T
import           GHC.Generics
import           Prelude                          hiding (foldl1, pure, (*>),
                                                   (<$), (<$>), (<*), (<*>),
                                                   (<|>))

import           Language.Haskell.Exts.Comments
import           Language.Haskell.Exts.ExactPrint
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
-- import           Language.Haskell.Exts.SimpleComments

import           Data.Struere.Parser
import           Data.Struere.Syntax
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
app = $(constructorIso 'App)
defineIsomorphisms ''QName


type HaskellFile = Module Info
type Info = (SrcSpanInfo, [Comment])

type Test = Exp Info
-- type Test = String

test :: Syntax f => f Test
test = fexpr
-- test = many alphaNum
test1 :: Test
-- test1 = "abc"
Right test1 = traceShowId $ runParser test $ T.pack "abcd"

instance Editable (Module (SrcSpanInfo, [Comment])) where
    structure = modulestr

    parser s = case parseModuleWithComments defaultParseMode $ T.unpack s of
        ParseOk (mod, comments) -> Right $ associateHaddock (mod, comments)
        ParseFailed loc s       -> Left s

    printer mod =
        T.pack . exactPrint (fmap fst mod) $ snd (ann mod)

modulestr :: Syntax f => f (Module (SrcSpanInfo, [Comment]))
modulestr = moduleiso <$> many decl

moduleiso :: Iso [Decl (SrcSpanInfo, [Comment])]
                 (Module (SrcSpanInfo, [Comment]))
moduleiso = isomorphism to from
  where
    to decls = Just $ Module (noSrcSpan, []) Nothing [] [] decls
    from (Module _ _ _ _ decls) = Just decls
    from _                      = Nothing

noInfo :: Syntax f => f Info
noInfo = pure (noSrcSpan, [])

decl :: Syntax f => f (Decl Info)
decl = funBind <$> noInfo <*> many1 matchc

matchc :: Syntax f => f (Match Info)
matchc = match <$> noInfo <*> empty <*> pure [] <*> rhs <*> pure Nothing

rhs :: Syntax f => f (Rhs Info)
rhs = unGuardedRhs <$> noInfo <*> expr


expr, fexpr, aexpr :: Syntax f => f (Exp Info)
expr = fexpr
fexpr = foldl1 (app `pap'` (noSrcSpan, [])) <$> many1 aexpr
aexpr = var <$> noInfo <*> qname

qname :: Syntax f => f (QName Info)
qname = unQual <$> noInfo <*> name

name :: Syntax f => f (Name Info)
name = ident <$> noInfo <*> identifier <* spaces

identifier :: Syntax f => f String
identifier = many1 alphaNum
             <?> "identifier"


name1 :: Name Info
name1 = Ident (noSrcSpan, []) "abcdef"


sepBy :: Syntax f => f () -> f a -> f [a]
sepBy = undefined

-- {-
-- BNF

-- value   ::= object | array | string | number | bool | 'null'
-- object  ::= '{' members '}'
-- members ::= | member (',' members)*
-- member  ::= string ':' value
-- array   ::= '[' values ']'
-- values  ::= | value (',' value)*
-- bool    ::= 'true' | 'false'
-- string  ::= '"' (character)* '"'
-- -}

-- data JSValue = JSObject [(String, JSValue)]
--              | JSArray [JSValue]
--              | JSString String
--              | JSNumber Float
--              | JSBool Bool
--              | JSNull

-- defineIsomorphisms ''JSValue

-- value   :: Syntax f => f JSValue
-- value   = jSObject <$> object
--       <|> jSArray  <$> array
--       <|> jSString <$> string
--       <|> jSNumber <$> number
--       <|> jSBool   <$> bool
--       <|> JSNull   <$  keyword "null"

-- object  :: Syntax f => f [(String, JSValue)]
-- object  = symbol "{" *> members <* symbol "}"

-- members :: Syntax f => f [(String, JSValue)]
-- members = sepBy (symbol ",") member

-- member  :: Syntax f => f (String, JSValue)
-- member  = string <*> symbol ":" *> value

-- array   :: Syntax f => f [JSValue]
-- array   = symbol "[" *> values <* symbol "]"

-- values  :: Syntax f => f [JSValue]
-- values  = sepBy (symbol ",") value

-- bool    :: Syntax f => f Bool
-- bool    = True  <$ symbol "true"
--       <|> False <$ symbol "false"

-- string  :: Syntax f => f String
-- string  = symbol "\"" *> many char <* symbol "\""
