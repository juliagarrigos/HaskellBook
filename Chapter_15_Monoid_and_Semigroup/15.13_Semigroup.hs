-- 15.13 Semigroup --

import Data.List.NonEmpty
import Data.Semigroup

--data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)

l = 1 :| [2, 3]
l2 = 1 :| [2]

l3 = l <> l2