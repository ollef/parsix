{-# LANGUAGE DefaultSignatures, GADTs #-}
module Text.Parsix.Combinators where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Monoid
import Data.Text(Text)
import qualified Data.Text.Unsafe as Unsafe
import Text.Parser.Combinators

import Text.Parsix.Parser
import Text.Parsix.Position
import Text.Parsix.Result

class Parsing m => SliceParsing m where
  slicedWith :: (a -> Text -> b) -> m a -> m b
  position :: m Position

  default position
    :: (MonadTrans t, Monad n, SliceParsing n, m ~ t n) => m Position
  position = lift position

instance SliceParsing Parser where
  slicedWith f p = do
    i <- position
    a <- p
    j <- position
    inp <- input
    return
      $ f a
      $ Unsafe.takeWord16 (codePoints j - codePoints i)
      $ Unsafe.dropWord16 (codePoints i) inp
    where
      input :: Parser Text
      input = Parser $ \s0 _s _e0 _e _pos inp -> s0 inp mempty

  position = Parser $ \s0 _s _e0 _e pos _inp -> s0 pos mempty

sliced :: SliceParsing m => m a -> m Text
sliced = slicedWith (\_ t -> t)

class Parsing m => RecoveryParsing m where
  withRecovery :: m a -> (ErrorInfo -> m a) -> m a

instance RecoveryParsing Parser where
  withRecovery (Parser p) recover = Parser
    $ \s0 s e0 e pos inp -> p
      s0
      s
      e0
      (\err pos' -> unParser (recover err)
        (\a err' -> s a (err <> err') pos')
        s
        (\err' -> e (err <> err') pos')
        e
        pos'
        inp)
      pos
      inp

-------------------------------------------------------------------------------
-- Boilerplate instances
instance (SliceParsing m, MonadPlus m) => SliceParsing (Lazy.StateT s m) where
  slicedWith f (Lazy.StateT m)
    = Lazy.StateT
    $ \s -> slicedWith (\(a, s') b -> (f a b, s')) $ m s

instance (SliceParsing m, MonadPlus m) => SliceParsing (Strict.StateT s m) where
  slicedWith f (Strict.StateT m)
    = Strict.StateT
    $ \s -> slicedWith (\(a, s') b -> (f a b, s')) $ m s

instance (SliceParsing m, MonadPlus m) => SliceParsing (ReaderT e m) where
  slicedWith f (ReaderT m) = ReaderT $ slicedWith f . m

instance (SliceParsing m, MonadPlus m, Monoid w) => SliceParsing (Strict.WriterT w m) where
  slicedWith f (Strict.WriterT m)
    = Strict.WriterT
    $ slicedWith (\(a,s') b -> (f a b, s')) m

instance (SliceParsing m, MonadPlus m, Monoid w) => SliceParsing (Lazy.WriterT w m) where
  slicedWith f (Lazy.WriterT m)
    = Lazy.WriterT
    $ slicedWith (\(a,s') b -> (f a b, s')) m

instance (SliceParsing m, MonadPlus m, Monoid w) => SliceParsing (Lazy.RWST r w s m) where
  slicedWith f (Lazy.RWST m)
    = Lazy.RWST
    $ \r s -> slicedWith (\(a, s', w) b -> (f a b, s', w)) $ m r s

instance (SliceParsing m, MonadPlus m, Monoid w) => SliceParsing (Strict.RWST r w s m) where
  slicedWith f (Strict.RWST m)
    = Strict.RWST
    $ \r s -> slicedWith (\(a, s', w) b -> (f a b, s', w)) $ m r s

instance (SliceParsing m, MonadPlus m) => SliceParsing (IdentityT m) where
  slicedWith f (IdentityT m) = IdentityT $ slicedWith f m

instance (RecoveryParsing m, MonadPlus m) => RecoveryParsing (Lazy.StateT s m) where
  withRecovery (Lazy.StateT m) r
    = Lazy.StateT
    $ \s -> m s `withRecovery` \err -> Lazy.runStateT (r err) s

instance (RecoveryParsing m, MonadPlus m) => RecoveryParsing (Strict.StateT s m) where
  withRecovery (Strict.StateT m) r
    = Strict.StateT
    $ \s -> m s `withRecovery` \err -> Strict.runStateT (r err) s

instance (RecoveryParsing m, MonadPlus m) => RecoveryParsing (ReaderT e m) where
  withRecovery (ReaderT m) r
    = ReaderT
    $ \s -> m s `withRecovery` \err -> runReaderT (r err) s

instance (RecoveryParsing m, MonadPlus m, Monoid w) => RecoveryParsing (Strict.WriterT w m) where
  withRecovery (Strict.WriterT m) r
    = Strict.WriterT
    $ m `withRecovery` (Strict.runWriterT . r)

instance (RecoveryParsing m, MonadPlus m, Monoid w) => RecoveryParsing (Lazy.WriterT w m) where
  withRecovery (Lazy.WriterT m) r
    = Lazy.WriterT
    $ m `withRecovery` (Lazy.runWriterT . r)

instance (RecoveryParsing m, MonadPlus m, Monoid w) => RecoveryParsing (Lazy.RWST r w s m) where
  withRecovery (Lazy.RWST m) r
    = Lazy.RWST
    $ \s s' -> m s s' `withRecovery` \err -> Lazy.runRWST (r err) s s'

instance (RecoveryParsing m, MonadPlus m, Monoid w) => RecoveryParsing (Strict.RWST r w s m) where
  withRecovery (Strict.RWST m) r
    = Strict.RWST
    $ \s s' -> m s s' `withRecovery` \err -> Strict.runRWST (r err) s s'

instance (RecoveryParsing m, MonadPlus m) => RecoveryParsing (IdentityT m) where
  withRecovery (IdentityT m) r
    = IdentityT
    $ m `withRecovery` (runIdentityT . r)