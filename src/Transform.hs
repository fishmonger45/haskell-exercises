{-# LANGUAGE InstanceSigs #-}

module Transform where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure . pure $ a

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a   
        -> Compose f g b
  (Compose f) <*> (Compose a) = 
    Compose $ ((<*>) <$> f) <*> a

instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


class BiFunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d 
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second f = bimap id f

data Deux a b = Deux a b

instance BiFunctor Deux where
  bimap :: (a -> b)
        -> (c -> d)
        -> Deux a c
        -> Deux b d 
  bimap f g (Deux a b) = Deux (f a) (g b) 

newtype Const a b = Const a

instance BiFunctor Const where
  bimap :: (a -> b)
        -> (c -> d)
        -> Const a c
        -> Const b d 
  bimap f _ (Const a) = Const $ f a 

data Drei a b c = Drei a b c

instance BiFunctor (Drei a) where
  bimap f g (Drei n a b) = Drei n (f a) (g b) 

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f 

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT hma) = MaybeT $ (fmap . fmap) f hma

instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ (pure . pure) a
  (MaybeT hmfab) <*> (MaybeT hma) = MaybeT $ ((<*>) <$> hmfab ) <*> hma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT $ f y

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a
  (EitherT f) <*> (EitherT a) = EitherT $ ((<*>) <$> f) <*> a

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f =
    EitherT $ do
      v <- ma
      case v of
        (Left y) -> return $ Left y
        (Right y) -> runEitherT $ f y

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right a) = Left a

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT mE) = EitherT $ swapEither <$> mE

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c

eitherT f g (EitherT x) = do
  y <- x
  case y of
    Left a -> f a
    Right b -> g b

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma 

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ pure $ pure a 
  (ReaderT fmab) <*> (ReaderT rma) =
    ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      x <- (rma r)
      runReaderT (f x) r

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  --fmap f (StateT smas) = StateT $ \s -> do
  --  (a, s1) <- smas s
  --  return $ ((f a), s1)

