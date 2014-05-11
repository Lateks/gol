module PlaneZip where

import qualified ListZip as LZ
import Control.Comonad

data PlaneZipper a = PZ (LZ.ListZipper (LZ.ListZipper a))

instance Functor PlaneZipper where
    fmap f (PZ z) = PZ (fmap (fmap f) z)

instance Comonad PlaneZipper where
    extract = pzRead
    duplicate z = PZ $ fmap horizontal $ vertical z

up :: PlaneZipper a -> PlaneZipper a
up (PZ z) = PZ (LZ.rewind z)

down :: PlaneZipper a -> PlaneZipper a
down (PZ z) = PZ (LZ.forward z)

left :: PlaneZipper a -> PlaneZipper a
left (PZ z) = PZ (fmap LZ.rewind z)

right :: PlaneZipper a -> PlaneZipper a
right (PZ z) = PZ (fmap LZ.forward z)

pzRead :: PlaneZipper a -> a
pzRead (PZ z) = LZ.lzRead $ LZ.lzRead z

replace :: a -> PlaneZipper a -> PlaneZipper a
replace x (PZ z) = PZ $ LZ.replace newLine z
    where newLine = (LZ.replace x) (LZ.lzRead z)

horizontal :: PlaneZipper a -> LZ.ListZipper (PlaneZipper a)
horizontal = LZ.move left right

vertical :: PlaneZipper a -> LZ.ListZipper (PlaneZipper a)
vertical = LZ.move up down
