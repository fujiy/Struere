
module Data.Struere.Structural where


import           Prelude hiding (id, (.), (<$>), pure, (<*>), (<*), (*>), (<|>))
import           Control.Category
import           Data.Typeable
import           Data.Maybe
import qualified Data.Char as C

import           Data.Struere.Isomorphism


-- Structural

data Level = Level

defaultLevel = Level

class (IsoFunctor f, ProductFunctor f, Alter f) => Structural f where
    pure :: Eq a => a -> f a
    char :: f Char

    sub  :: Typeable a => Level -> f a -> f a
    sub l = id

    label :: String -> f a -> f a
    label _ x = x

-- Combinators

many :: Structural f => f a -> f [a]
many p = cons <$> p <*> many p
     <|> nil <$> pure ()

many' :: (Structural f, Eq a) => f a -> f [a]
many' p = ( cons <$> p <*> many' p )
     <|> nil <$> pure ()

many1 :: Structural f => f a -> f [a]
many1 p = cons <$> p <*> many p

infixl 4 <+>

(<+>) :: Structural f => f a -> f b -> f (Either a b)
p <+> q = (left <$> p) <|> (right <$> q)

(*>) :: Structural f => f () -> f a -> f a
p *> q = inverse unit . commute <$> p <*> q

(<*) :: Structural f => f a -> f () -> f a
p <* q = inverse unit <$> p <*> q

between :: Structural f => f () -> f () -> f a -> f a
between p q r = p *> r <* q

-- Basics

satisfy :: Structural f => (Char -> Bool) -> f Char
satisfy f = inverse (subset f) <$> char

charactor :: Structural f => Char -> f Char
charactor c = satisfy (c ==)

text :: Structural f => String -> f ()
text = foldr
    (\c p -> ignore ((), ())
         <$> (ignore c <$> charactor c)
         <*> p)
    (pure ())

spaces   :: Structural f => f ()
spaces   = void <$> many (charactor ' ')

upper    :: Structural f => f Char
upper    = satisfy C.isUpper

lower    :: Structural f => f Char
lower    = satisfy C.isLower

alphaNum :: Structural f => f Char
alphaNum = satisfy C.isAlphaNum

letter   :: Structural f => f Char
letter   = satisfy C.isAlpha

digit    :: Structural f => f Char
digit    = satisfy C.isDigit
