module Grammar where

import Parser
import Syntax (Syntax, SyntaxElement(Nonterminal, Terminal, NoneOf), Grammar, palphabet)
import Interpolation (member, Object(Object))

members :: Eq a => [(a, b)] -> a -> [b]
members l e = map snd $ filter (\x -> e == fst x) l

parseSyntaxElement :: Grammar -> SyntaxElement -> Parser [(String, Object)]
parseSyntaxElement g (Nonterminal attrname nont) = pmap (\x -> [(attrname, x)]) (parseNonterminal nont g)
parseSyntaxElement g (Terminal t) = pmap (const []) $ token t
parseSyntaxElement _ (NoneOf n)   = pmap (const []) $ noneOf n

parseSyntax :: Grammar -> Syntax -> Parser [(String, Object)]
parseSyntax _ [] = pconst []
parseSyntax g syn = let
        mergeParsers :: Parser [(String, Object)] -> Parser [(String, Object)] -> Parser [(String, Object)]
        mergeParsers a b = pmap (uncurry (++)) (a <++> b)
    in foldl1 mergeParsers $ map (parseSyntaxElement g) syn

_parseNonterminal :: String -> Grammar -> Parser Object
_parseNonterminal nont x s = do
        (a, t) <- (foldl (<||>) pfalse $ map (parseSyntax x) $ members x nont) s
        return (Object nont (take (length s - length t) s) a, t)

parseNonterminal :: String -> Grammar -> Parser Object
parseNonterminal n g = pStack n $ _parseNonterminal n g

parseGrammar :: Grammar -> Parser Object
parseGrammar = parseNonterminal "root"
