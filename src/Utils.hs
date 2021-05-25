module Utils (
  Error
, (!!?)
) where

type Error = String

(!!?) :: [a] -> Int -> Maybe a
xs !!? n | n >= 0 && n < length xs = Just $ xs !! n
_  !!? _                           = Nothing
