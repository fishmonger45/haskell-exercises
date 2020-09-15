import Data.Monoid
import Data.Foldable

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a 

instance Foldable Identity where
  foldr f z (Identity x) = f x z

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

data Optional a = 
    Nada
  | Yep a

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a 

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (f <$> rest)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a rest) = (f a) <> (foldMap f rest)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a rest) = Cons <$> f a <*> traverse f rest

data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b 

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair <$> (pure a) <*> (f b)

data Three a b c =
  Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

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

data Big a b =
  Big a b b

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap f (Big a b c) = (f b) <> (f c)

instance Traversable (Big a) where
  traverse f (Big a b c) = Big a <$> f b <*> f c





