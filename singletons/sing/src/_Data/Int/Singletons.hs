
module Data.Int.Singletons
    ( Word8' (..)
    , SWord8 (..)
    ) where

import Data.Bits (testBit, (.|.))
import Data.Bool.Singletons (SBool (..))
import Data.Eq.Singletons (SEq (..))
import Data.Ord.Singletons (SOrd (..))
import Data.Singletons (Sing, SingKind (..), SomeSing (..))
import Data.Word (Word8)

data Word8' = Word8' !Bool !Bool !Bool !Bool !Bool !Bool !Bool !Bool

data SWord8 w where
    SWord8
        :: Sing b0 -> Sing b1 -> Sing b2 -> Sing b3
        -> Sing b4 -> Sing b5 -> Sing b6 -> Sing b7
        -> SWord8 ('Word8' b0 b1 b2 b3 b4 b5 b6 b7)

type instance Sing = SWord8

instance SingKind Word8' where
    type Demote Word8' = Word8

    toSing w =
        let (b0, b1, b2, b3, b4, b5, b6, b7) =
                ( testBit w 0, testBit w 1, testBit w 2, testBit w 3
                , testBit w 4, testBit w 5, testBit w 6, testBit w 7)
         in case (toSing b0, toSing b1, toSing b2, toSing b3, toSing b4, toSing b5, toSing b6, toSing b7) of
            (SomeSing b0', SomeSing b1', SomeSing b2', SomeSing b3', SomeSing b4', SomeSing b5', SomeSing b6', SomeSing b7') -> SomeSing (SWord8 b0' b1' b2' b3' b4' b5' b6' b7')
        
    fromSing (SWord8 b0 b1 b2 b3 b4 b5 b6 b7) =
            (if fromSing b0 then 0x01 else 0x00)
        .|. (if fromSing b1 then 0x02 else 0x00)
        .|. (if fromSing b2 then 0x04 else 0x00)
        .|. (if fromSing b3 then 0x08 else 0x00)
        .|. (if fromSing b4 then 0x10 else 0x00)
        .|. (if fromSing b5 then 0x20 else 0x00)
        .|. (if fromSing b6 then 0x40 else 0x00)
        .|. (if fromSing b7 then 0x80 else 0x00)

type family EqWord8' x y where
    EqWord8' ('Word8' a b c d e f g h) ('Word8' a b c d e f g h) = 'True
    EqWord8' ('Word8' 'True _ _ _ _ _ _ _)  ('Word8' 'False _ _ _ _ _ _ _) = 'False
    EqWord8' ('Word8' 'False _ _ _ _ _ _ _) ('Word8' 'True _ _ _ _ _ _ _) = 'False
    EqWord8' ('Word8' _ 'True _ _ _ _ _ _)  ('Word8' _ 'False _ _ _ _ _ _) = 'False
    EqWord8' ('Word8' _ 'False _ _ _ _ _ _) ('Word8' _ 'True _ _ _ _ _ _)  = 'False
    EqWord8' ('Word8' _ _ 'True _ _ _ _ _)  ('Word8' _ _ 'False _ _ _ _ _) = 'False
    EqWord8' ('Word8' _ _ 'False _ _ _ _ _) ('Word8' _ _ 'True _ _ _ _ _)  = 'False
    EqWord8' ('Word8' _ _ _ 'True _ _ _ _)  ('Word8' _ _ _ 'False _ _ _ _) = 'False
    EqWord8' ('Word8' _ _ _ 'False _ _ _ _) ('Word8' _ _ _ 'True _ _ _ _)  = 'False
    EqWord8' ('Word8' _ _ _ _ 'True _ _ _)  ('Word8' _ _ _ _ 'False _ _ _) = 'False
    EqWord8' ('Word8' _ _ _ _ 'False _ _ _) ('Word8' _ _ _ _ 'True _ _ _)  = 'False
    EqWord8' ('Word8' _ _ _ _ _ 'True _ _)  ('Word8' _ _ _ _ _ 'False _ _) = 'False
    EqWord8' ('Word8' _ _ _ _ _ 'False _ _) ('Word8' _ _ _ _ _ 'True _ _)  = 'False
    EqWord8' ('Word8' _ _ _ _ _ _ 'True _)  ('Word8' _ _ _ _ _ _ 'False _) = 'False
    EqWord8' ('Word8' _ _ _ _ _ _ 'False _) ('Word8' _ _ _ _ _ _ 'True _)  = 'False
    EqWord8' ('Word8' _ _ _ _ _ _ _ 'True)  ('Word8' _ _ _ _ _ _ _ 'False) = 'False
    EqWord8' ('Word8' _ _ _ _ _ _ _ 'False) ('Word8' _ _ _ _ _ _ _ 'True)  = 'False

instance SEq Word8' where
    type (==) a b = EqWord8' a b

    (SWord8 STrue _ _ _ _ _ _ _)  %== (SWord8 SFalse _ _ _ _ _ _ _) = SFalse
    (SWord8 SFalse _ _ _ _ _ _ _) %== (SWord8 STrue _ _ _ _ _ _ _)  = SFalse
    (SWord8 _ STrue _ _ _ _ _ _)  %== (SWord8 _ SFalse _ _ _ _ _ _) = SFalse
    (SWord8 _ SFalse _ _ _ _ _ _) %== (SWord8 _ STrue _ _ _ _ _ _)  = SFalse
    (SWord8 _ _ STrue _ _ _ _ _)  %== (SWord8 _ _ SFalse _ _ _ _ _) = SFalse
    (SWord8 _ _ SFalse _ _ _ _ _) %== (SWord8 _ _ STrue _ _ _ _ _)  = SFalse
    (SWord8 _ _ _ STrue _ _ _ _)  %== (SWord8 _ _ _ SFalse _ _ _ _) = SFalse
    (SWord8 _ _ _ SFalse _ _ _ _) %== (SWord8 _ _ _ STrue _ _ _ _)  = SFalse
    (SWord8 _ _ _ _ STrue _ _ _)  %== (SWord8 _ _ _ _ SFalse _ _ _) = SFalse
    (SWord8 _ _ _ _ SFalse _ _ _) %== (SWord8 _ _ _ _ STrue _ _ _)  = SFalse
    (SWord8 _ _ _ _ _ STrue _ _)  %== (SWord8 _ _ _ _ _ SFalse _ _) = SFalse
    (SWord8 _ _ _ _ _ SFalse _ _) %== (SWord8 _ _ _ _ _ STrue _ _)  = SFalse
    (SWord8 _ _ _ _ _ _ STrue _)  %== (SWord8 _ _ _ _ _ _ SFalse _) = SFalse
    (SWord8 _ _ _ _ _ _ SFalse _) %== (SWord8 _ _ _ _ _ _ STrue _)  = SFalse
    (SWord8 _ _ _ _ _ _ _ STrue)  %== (SWord8 _ _ _ _ _ _ _ SFalse) = SFalse
    (SWord8 _ _ _ _ _ _ _ SFalse) %== (SWord8 _ _ _ _ _ _ _ STrue)  = SFalse
    (SWord8 _ _ _ _ _ _ _ _) %== (SWord8 _ _ _ _ _ _ _ _)  = undefined

-- data Bits32 = Bits32 !Bits8 !Bits8 !Bits8 !Bits8

-- data SBits32 b where
--     SBits32
--         :: SBit b0 -> SBit b1 -> SBit b2 -> SBit b3
--         -> SBit b4 -> SBit b5 -> SBit b6 -> SBit b7
--         -> SBit b8 -> SBit b9 -> SBit b10 -> SBit b11
--         -> SBit b12 -> SBit b13 -> SBit b14 -> SBit b15
--         -> SBit b16 -> SBit b17 -> SBit b18 -> SBit b19
--         -> SBit b20 -> SBit b21 -> SBit b22 -> SBit b23
--         -> SBit b24 -> SBit b25 -> SBit b26 -> SBit b27
--         -> SBit b28 -> SBit b29 -> SBit b30 -> SBit b31
--         -> SBits32 ('Bits32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b21)

-- type instance Sing = SBits32


