module Chp26Notes where
import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mMa) = MaybeT $ (fmap . fmap) f mMa

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure 
  (MaybeT mMf) <*> (MaybeT mMa) = MaybeT $ (<*>) <$> mMf <*> mMa

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT mMa) >>= f = MaybeT $ mMa >>= runFunc
    where runFunc (Just a) = runMaybeT $ f a; 
          runFunc Nothing = return Nothing

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just
    
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mEa) = EitherT $ (fmap . fmap) f mEa

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT mEf) <*> (EitherT mEa) = EitherT $ (<*>) <$> mEf <*> mEa

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mEa) >>= f = EitherT $ mEa >>= runFunc
    where runFunc (Left a) = return $ Left a
          runFunc (Right b) = (runEitherT . f) b

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mEa) = EitherT $ fmap swapEither mEa

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mEa) = mEa >>= either f g

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT g) = ReaderT $ (fmap . fmap) f g

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT mRf) <*> (ReaderT mRa) = ReaderT $ (<*>) <$> mRf <*> mRa

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT mRa) >>= f = ReaderT $ \r -> do
    a <- mRa r
    runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

newtype FirstTuple a b = FirstTuple { getTuple :: (b, a) } deriving (Eq, Show)

instance Functor (FirstTuple a) where
  fmap f (FirstTuple (a, b)) = FirstTuple (f a, b)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \s -> fmap (getTuple . fmap f . FirstTuple) (g s)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (StateT sMf) <*> (StateT sMa) = StateT $ \s -> do
    (f, s1) <- sMf s
    (a, s2) <- sMa s1
    return (f a, s2)
  
instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT sMa) >>= f = StateT $ \s -> do
    (a, s1) <- sMa s
    (b, s2) <- runStateT (f a) s1
    return (b, s2)

instance MonadTrans(StateT s) where
  lift m = StateT $ \s -> flip (,) s <$> m

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO =  lift . liftIO
