module TorusZip where

import CyclicZip as CZ

import Control.Comonad

data TorusZipper a = TZ (CZ.CyclicZipper (CZ.CyclicZipper a))
    deriving (Show)

instance Functor TorusZipper where
    fmap f (TZ z) = TZ (fmap (fmap f) z)

instance Comonad TorusZipper where
    extract = tzRead
    duplicate z = TZ $ fmap horizontal $ vertical z

tzHeight :: TorusZipper a -> Int
tzHeight (TZ z) = CZ.czLength z

tzWidth :: TorusZipper a -> Int
tzWidth (TZ z) = extract length
    where length = fmap CZ.czLength z

up :: TorusZipper a -> TorusZipper a
up (TZ z) = TZ (CZ.rewind z)

down :: TorusZipper a -> TorusZipper a
down (TZ z) = TZ (CZ.forward z)

left :: TorusZipper a -> TorusZipper a
left (TZ z) = TZ (fmap CZ.rewind z)

right :: TorusZipper a -> TorusZipper a
right (TZ z) = TZ (fmap CZ.forward z)

tzRead :: TorusZipper a -> a
tzRead (TZ z) = CZ.czRead $ CZ.czRead z

replace :: a -> TorusZipper a -> TorusZipper a
replace x (TZ z) = TZ $ CZ.replace (CZ.replace x (CZ.czRead z)) z

horizontal :: TorusZipper a -> CZ.CyclicZipper (TorusZipper a)
horizontal = CZ.shift tzWidth right

vertical :: TorusZipper a -> CZ.CyclicZipper (TorusZipper a)
vertical = CZ.shift tzHeight down

toList :: TorusZipper a -> [[a]]
toList (TZ cz) = map CZ.toList $ CZ.toList cz
