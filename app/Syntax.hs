module Syntax where

import Data.Foldable (fold)
import Parser

data SyntaxElement = Nonterminal String String | Terminal String | NoneOf String deriving Show

type Syntax = [SyntaxElement]
type Grammar = [(String, Syntax)]

data InterpolationElement = Attribute [String] | Text String deriving Show
type Interpolation = [InterpolationElement]
type InterpolationLibrary = [(String, Interpolation)]

data Language = Lang { grammar :: Grammar
                     , ilib :: InterpolationLibrary
                     }

instance Semigroup Language where
    (Lang g il) <> (Lang g' il') = Lang (g ++ g') (il ++ il')

instance Monoid Language where
    mempty = Lang [] []

-- general parsers
pwhitespace :: Parser String
pwhitespace = pStack "whitespace" $ prepeat $ oneOf " "

palphabet :: Parser Char
palphabet = pStack "alphabet" $ oneOf (enumFromTo 'A' 'Z' ++ enumFromTo 'a' 'z')

psymbol :: Parser String
psymbol = pStack "symbol" $ prepeat palphabet

-- PATTERNS
pnonterminal :: Parser SyntaxElement
pnonterminal = let
        pnamed = pmap (uncurry Nonterminal) $
            psymbol <<< pwhitespace <<< token ":" <<< pwhitespace <++> psymbol
        punnamed = pmap (Nonterminal "") psymbol
        pinner = pnamed <||> punnamed
    in pStack "nonterminal bracket" $ token "[" >>> pwhitespace >>> pinner <<< pwhitespace <<< token "]"

pnoneofbracket :: Parser SyntaxElement
pnoneofbracket = let
    in pStack "NoneOfBracket" $ pmap (NoneOf . concat) $ token "[^" >>> prepeat (punescaped "\n]" <||> pescaped <||> pnewline) <<< token "]"

punescaped specials = nonempty $ prepeat (noneOf $ "\\" ++ specials)
pescaped = pmap (:[]) $ token "\\" >>> noneOf "n"
pnewline = pmap (const "\n") $ token "\\n"

pterminal :: Parser SyntaxElement
pterminal = pStack "terminal literal" $ pmap (Terminal . concat) . nonempty $ prepeat (punescaped "[\n" <||> pescaped <||> pnewline)

psyntax :: Parser Syntax
psyntax = pStack "syntax pattern" $ prepeat (pnonterminal <||> pterminal <||> pnoneofbracket)

pdecl :: Parser (String, Syntax)
pdecl = pStack "nonterminal declaration" $ psymbol <<< pwhitespace <<< token "::=" <<< pwhitespace <++> psyntax <<< token "\n"

-- INTERPOLATIONS
pattributename :: Parser [String]
pattributename = pStack "attribute name" $ pmap (uncurry (:)) $
    psymbol <++>
    prepeat (token "." >>> psymbol)

pattribute :: Parser InterpolationElement
pattribute = pStack "interpolation bracket" $ pmap Attribute $ token "[" >>> pwhitespace >>> pattributename <<< pwhitespace <<< token "]"

ptext :: Parser InterpolationElement
ptext = pmap (\(Terminal x) -> Text x) pterminal

pinterpolation :: Parser Interpolation
pinterpolation = pStack "interpolation pattern" $ prepeat $ pattribute <||> ptext

pmacro :: Parser (String, Interpolation)
pmacro = pStack "macro declaration" $ psymbol <<< pwhitespace <<< token ":=" <<< pwhitespace <++> pinterpolation <<< token "\n"

-- parse complete mocca libraries
plib :: Parser Language
plib = pStack "language" $ pmap fold . prepeat $
    pmap (\x -> Lang [x] []) pdecl <||>
    pmap (\x -> Lang [] [x]) pmacro <||>
    pmap (const $ Lang [] []) (token "\n")
