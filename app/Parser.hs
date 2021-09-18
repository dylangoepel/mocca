module Parser where

import Data.Bifunctor (first)
import GHC.Base (liftA2)

-- Generic Parser Combinator Functions

data PMaybe a = PJust a | PErr [String]

instance Functor PMaybe where
    fmap f (PJust x) = PJust $ f x
    fmap f (PErr ctx) = PErr ctx

instance Applicative PMaybe where
    pure x = PJust x
    liftA2 f (PJust x) (PJust y) = PJust (f x y)
    liftA2 f (PErr ctx) _ = PErr ctx
    liftA2 f (PJust x) (PErr ctx) = PErr ctx

instance Monad PMaybe where
    (PJust x) >>= f = f x
    (PErr ctx) >>= f = PErr ctx

type Parser a = String -> PMaybe (a, String)

pStack :: String -> Parser a -> Parser a
pStack name p s = case p s of 
    PErr stack -> PErr $ name : stack
    x -> x

(<||>) :: Parser a -> Parser a -> Parser a
(p <||> q) s = case p s of
    PErr _ -> q s
    x -> x 

(<++>) :: Parser a -> Parser b -> Parser (a, b)
(p <++> q) s = do
    (rp, tp) <- p s
    (rq, tq) <- q tp
    return ((rp, rq), tq)

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = fmap (first f) . p

(>>>) :: Parser a -> Parser b -> Parser b
p >>> q = pmap snd $ p <++> q

(<<<) :: Parser a -> Parser b -> Parser a
p <<< q = pmap fst $ p <++> q

prepeat :: Parser a -> Parser [a]
prepeat p = let
    _repeat :: [a] -> Parser a -> Parser [a]
    _repeat xs p s = case p s of
        PErr _ -> PJust (xs, s)
        PJust (r, t) -> _repeat (r : xs) p t
    in pmap reverse $ _repeat [] p

-- Simple parsers
oneOf :: String -> Parser Char
oneOf _ [] = PErr []
oneOf alph (x:xs)
    | x `elem` alph = PJust (x, xs)
    | otherwise     = PErr []

noneOf :: String -> Parser Char
noneOf _ [] = PErr []
noneOf alph (x:xs)
    | x `elem` alph = PErr []
    | otherwise     = PJust (x, xs)

nonempty :: (Monoid a, Eq a) => Parser a -> Parser a
nonempty p s = do
    (r, t) <- p s
    if r == mempty then
        PErr []
    else
        return (r, t)

token :: String -> Parser String
token t s = let
        hasPrefix _ [] = True
        hasPrefix [] _ = False
        hasPrefix (x:xs) (p:ps)
            | p == x    = hasPrefix xs ps
            | otherwise = False
    in if s `hasPrefix` t then
        return (t, drop (length t) s)
    else
        PErr []

pfalse :: Parser a
pfalse _ = PErr []

pconst :: a -> Parser a
pconst a s = PJust (a, s)
