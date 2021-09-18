module Interpolation where

import Syntax

data Object = Object { nonterminal :: String
                     , text :: String
                     , attrs :: [(String, Object)]
                     }

member :: Eq a => [(a, b)] -> a -> Maybe b
member [] _ = Nothing
member ((a, b):xs) c
    | c == a = Just b
    | otherwise = member xs c

hasMember :: Eq a => [(a, b)] -> a -> Bool
hasMember l x = case member l x of
    Just _  -> True
    Nothing -> False

attr :: Object -> [String] -> Maybe Object
attr o [] = Just o
attr o (a:as) = member (attrs o) a >>= flip attr as

_interpolateElement :: InterpolationLibrary -> Object -> InterpolationElement -> Maybe String
_interpolateElement lib ctx (Attribute a) = do
    o <- attr ctx a
    return $ interpolate lib o
_interpolateElement _ _ (Text s) = Just s

_interpolate :: InterpolationLibrary -> Object -> Maybe Interpolation -> String
_interpolate lib o (Just xs) = case mapM (_interpolateElement lib o) xs of
    Just x -> concat x
    Nothing -> text o
_interpolate _ o Nothing = text o

interpolate :: InterpolationLibrary -> Object -> String
interpolate lib o = _interpolate lib o (member lib $ nonterminal o)
