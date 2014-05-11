module CyclicZip where

import Control.Comonad

data CyclicZipper a = CZ [a]
    deriving (Show)

instance Functor CyclicZipper where
    fmap f (CZ a) = CZ (map f a)

instance Comonad CyclicZipper where
    extract = czRead
    duplicate = shift czLength forward

czLength :: CyclicZipper a -> Int
czLength (CZ xs) = length xs

forward :: CyclicZipper a -> CyclicZipper a
forward (CZ (x:xs)) = CZ (xs ++ [x])
forward _           = error "Empty zipper"

rewind :: CyclicZipper a -> CyclicZipper a
rewind (CZ xs) = if (length xs > 0)
                    then CZ $ (last xs):(init xs)
                    else error "Empty zipper"

czRead :: CyclicZipper a -> a
czRead (CZ (x:xs)) = x
czRead _           = error "Empty zipper"

replace :: a -> CyclicZipper a -> CyclicZipper a
replace x (CZ (y:ys)) = CZ (x:ys)

shift :: (z a -> Int) -> (z a -> z a) -> z a -> CyclicZipper (z a)
shift f g z = CZ (take n shifted)
    where n = f z
          shifted = iterate g z

toList :: CyclicZipper a -> [a]
toList (CZ l) = l
