
import Data.Generics

import Data.Binary

import BinaryDerive

data Foo = Bar
    deriving (Typeable, Data, Show, Eq)

instance Binary Main.Foo where
  put Bar = return ()
  get = return Bar

data Color = RGB Int Int Int
           | CMYK Int Int Int Int
    deriving (Typeable, Data, Show, Eq)

instance Binary Main.Color where
  put (RGB a b c) = putWord8 0 >> put a >> put b >> put c
  put (CMYK a b c d) = putWord8 1 >> put a >> put b >> put c >> put d
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> return (RGB a b c)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CMYK a b c d)

data Computer = Laptop { weight :: Int }
              | Desktop { speed :: Int, memory :: Int }
    deriving (Typeable, Data, Show, Eq)

instance Binary Main.Computer where
  put (Laptop a) = putWord8 0 >> put a
  put (Desktop a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (Laptop a)
      1 -> get >>= \a -> get >>= \b -> return (Desktop a b)

-- | All drinks mankind will ever need
data Drinks = Beer Bool{-ale?-}
            | Coffee
            | Tea
            | EnergyDrink
            | Water
            | Wine
            | Whisky
    deriving (Typeable, Data, Show, Eq)

instance Binary Main.Drinks where
  put (Beer a) = putWord8 0 >> put a
  put Coffee = putWord8 1
  put Tea = putWord8 2
  put EnergyDrink = putWord8 3
  put Water = putWord8 4
  put Wine = putWord8 5
  put Whisky = putWord8 6
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (Beer a)
      1 -> return Coffee
      2 -> return Tea
      3 -> return EnergyDrink
      4 -> return Water
      5 -> return Wine
      6 -> return Whisky
