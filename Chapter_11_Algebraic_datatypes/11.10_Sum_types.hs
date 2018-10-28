-- 11.10 Sum types --

import Data.Int

-- Exercises: Pity the Bool

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

myNumba :: NumberOrBool
myNumba = Numba (-127)
