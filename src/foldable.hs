import Data.Monoid
import Data.Foldable

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z

data Optional a = 
    Nada
  | Yep a

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (\x acc -> x + acc) 0 

sum'' :: (Foldable t, Num a) => t a -> a
sum'' xs = getSum $ foldMap (\x -> Sum x) xs

product' :: (Foldable t, Num a) => t a -> a
product' xs = getProduct $ foldMap (\x -> Product x) xs

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' s xs = getAny $ foldMap (\y -> Any $ y == s) xs

null' :: Foldable t => t a -> Bool
null' xs = (length' xs) == 0

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' xs = foldr (\x acc -> x:acc) [] xs

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x acc -> (f x) <> acc) mempty xs

data Constant a b = 
  Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where 
  foldMap _ _ = mempty

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b 

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c)

data Four' a b =
  Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f xs = foldMap (\x -> if f x then pure x else mempty) xs



