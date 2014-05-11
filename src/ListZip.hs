module ListZip where

import Control.Comonad

data ListZipper a = LZ [a] [a]
    deriving (Show)

instance Functor ListZipper where
    fmap f (LZ ls rs) = LZ (map f ls) (map f rs)

instance Comonad ListZipper where
    extract = lzRead
    duplicate = move rewind forward

forward :: ListZipper a -> ListZipper a
forward (LZ ls (r:rs)) = LZ (r:ls) rs

rewind :: ListZipper a -> ListZipper a
rewind (LZ (l:ls) rs) = LZ ls (l:rs)

move :: (z a -> z a) -> (z a -> z a) -> z a -> ListZipper (z a)
move f g z = LZ (tail . iterate f $ z) (iterate g z)

lzRead :: ListZipper a -> a
lzRead (LZ _ (r:rs)) = r

safeLzRead :: ListZipper a -> Maybe a
safeLzRead (LZ _ (r:rs)) = Just r
safeLzRead (LZ _ _)      = Nothing

replace :: a -> ListZipper a -> ListZipper a
replace x (LZ ls (r:rs)) = LZ ls (x:rs)

toList :: ListZipper a -> Int -> [a]
toList (LZ ls rs) n = reverse (take n ls) ++ (take n rs)
