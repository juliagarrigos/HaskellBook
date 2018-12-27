-- 15.10 Reusing algebras by asking for algebras --

import Data.Monoid

-- Exercise: Optional Monoid
data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) = (<>)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only x) (Only y) = Only(mappend x y)
    mappend (Only x) Nada = Only x
    mappend Nada (Only y) = Only y
    mappend Nada Nada = Nada