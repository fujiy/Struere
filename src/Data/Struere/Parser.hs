
module Data.Struere.Parser where

import           Control.Monad
import qualified Data.Text                as T

import           Data.Struere.Isomorphism hiding ((.))
import           Data.Struere.Syntax

newtype Parser a = Parser (T.Text -> Maybe (a, T.Text))

instance IsoFunctor Parser where
    iso <$> Parser p
        = Parser $ \s -> do
            (a, s') <- p s
            b <- apply iso a
            return (b, s')

instance ProductFunctor Parser where
    Parser p <*> Parser q
        = Parser $ \s -> do
            (a, s')  <- p s
            (b, s'') <- q s'
            return ((a, b), s'')

instance Alter Parser where
    Parser p <|> Parser q
        = Parser $ \s -> p s `mplus` q s
    empty = Parser $ const Nothing

instance Syntax Parser where
    pure a = Parser $ \s -> Just (a, s)
    char   = Parser T.uncons

runParser :: Parser a -> T.Text -> Either String a
runParser (Parser p) s = maybe (Left "parse error") (Right . fst) (p s)
