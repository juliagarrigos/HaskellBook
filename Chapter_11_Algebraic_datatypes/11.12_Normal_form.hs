-- 11.12 Normal form --

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show
type AuthorName = String

data Author = Author (AuthorName, BookType)

-- The same as Author but in Normal Form
data Author' = Fiction' AuthorName | NonFictionBook' AuthorName deriving (Eq, Show)

-- Exercises: How Does Your Garden Grow?
data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show
type Gardener = String
data Garden = Garden Gardener FlowerType deriving Show

data Garden' = Gardenia' Gardener | Daisy' Gardener | Rose' Gardener | Lilac' Gardener