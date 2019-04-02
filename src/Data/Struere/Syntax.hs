{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Struere.Syntax where


import           Control.Category
import qualified Data.Char                as C
import           Data.Functor.Product
import           Data.Maybe
import           Data.Typeable
import           Prelude                  hiding (id, pure, (*>), (.), (<$),
                                           (<$>), (<*), (<*>), (<|>))

import           Data.Struere.Isomorphism


-- Syntax

data Level = Level

defaultLevel = Level


infixl 3 <|>
infixr 6 <*>

-- |Classes

class ProductFunctor f where
    (<*>) :: f alpha -> f beta -> f (alpha, beta)

class Alter f where
    empty :: f alpha
    (<|>) :: f alpha -> f alpha -> f alpha

class ProductFunctor f => InverseProductFunctor f where
    unproduct :: f (alpha, beta) -> (f alpha, f beta)

class (IsoFunctor f, ProductFunctor f, Alter f) => Syntax f where
    pure :: Eq a => a -> f a
    char :: f Char

    -- sub  :: Typeable a => Level -> f a -> f a
    -- sub l x = x

    -- label :: String -> f a -> f a
    -- label _ x = x

    part :: Typeable a => Description -> f a -> f a
    part _ x = x


instance (ProductFunctor f, ProductFunctor g) =>
    ProductFunctor (Product f g) where
    Pair fa ga <*> Pair fb gb = Pair (fa <*> fb) (ga <*> gb)

instance (Alter f, Alter g) => Alter (Product f g) where
    empty                     = Pair empty empty
    Pair fa ga <|> Pair fb gb = Pair (fa <|> fb) (ga <|> gb)

instance (Syntax f, Syntax g, IsoFunctor (Product f g)) =>
    Syntax (Product f g) where
    pure a            = Pair (pure a)   (pure a)
    char              = Pair char       char
    part d (Pair x y) = Pair (part d x) (part d y)
    -- Pair fa ga <|> Pair fb gb = Pair (fa <|> fb) (ga <|> gb)



data Description = Description
    { grid       :: Grid
    , isOrnament :: Bool
    , label      :: Maybe String
    , style      :: Style
    }

defDesc :: Description
defDesc = Description Column False Nothing Plane

data Grid = Column
          | Row

data Style = Plane
           | Keyword
           | Operator


-- Poly

data Poly c f a = Poly
    { poly :: forall g. c g => g a
    , mono :: f a
                       -- runPoly :: (forall g. c g => g a) -> f a
    }



-- Combinators

many :: Syntax f => f a -> f [a]
-- many p = cons <$> p <*> many p
--      <|> nil <$> pure ()

many p = let many_p = cons <$> p <*> many_p
                 <|> nil <$> pure ()
         in  many_p

many1 :: Syntax f => f a -> f [a]
many1 p = cons <$> p <*> many p

option :: Syntax f => f a -> f (Maybe a)
option p = just <$> p
       <|> nothing <$> pure ()

infix  0 <#>
infix  0 <?>
infixl 4 <+>
infixl 4 <$
infixl 4 $>

(<+>) :: Syntax f => f a -> f b -> f (Either a b)
p <+> q = (left <$> p) <|> (right <$> q)

(*>) :: Syntax f => f () -> f a -> f a
p *> q = inverse unit . commute <$> p <*> q

(<*) :: Syntax f => f a -> f () -> f a
p <* q = inverse unit <$> p <*> q

(<$) :: IsoFunctor f => a -> f () -> f a
a <$ p = inverse (ignore a) <$> p

($>) :: IsoFunctor f => f () -> a -> f a
($>) = flip (<$)

not :: Syntax f => f () -> f ()
not p = ignore True . subset id <$> (False <$ p <|> pure True)

between :: Syntax f => f () -> f () -> f a -> f a
between p q r = p *> r <* q

(<#>) :: (Typeable a, Syntax f) => f a -> Description -> f a
(<#>) = flip part

(<?>) :: (Typeable a, Syntax f) => f a -> String -> f a
p <?> l = part (defDesc { label = Just l }) p

-- Basics

satisfy :: Syntax f => (Char -> Bool) -> f Char
satisfy f = inverse (subset f) <$> char

charactor :: Syntax f => Char -> f Char
charactor c = satisfy (c ==)

text :: Syntax f => String -> f ()
text = foldr
    (\c p -> ignore ((), ())
         <$> (ignore c <$> charactor c)
         <*> p)
    (pure ())

spaces   :: Syntax f => f ()
spaces   = ignore " " <$> many (charactor ' ')
           <#> defDesc { isOrnament = True }

upper    :: Syntax f => f Char
upper    = satisfy C.isUpper

lower    :: Syntax f => f Char
lower    = satisfy C.isLower

alphaNum :: Syntax f => f Char
alphaNum = satisfy C.isAlphaNum

letter   :: Syntax f => f Char
letter   = satisfy C.isAlpha

digit    :: Syntax f => f Char
digit    = satisfy C.isDigit

