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
import Data.Text(Text)
import qualified Data.Text.Unsafe as Unsafe
import Text.Parser.Combinators

import Text.Parsix.Highlight
import Text.Parsix.Parser.Internal
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
      $ Unsafe.takeWord16 (codeUnits j - codeUnits i)
      $ Unsafe.dropWord16 (codeUnits i) inp

  position = Parser $ \s0 _s _e0 _e pos _hl _inp -> s0 pos mempty

sliced :: SliceParsing m => m a -> m Text
sliced = slicedWith (\_ t -> t)

class Parsing m => RecoveryParsing m where
  withRecovery :: (ErrorInfo -> Position -> m a) -> m a -> m a

instance RecoveryParsing Parser where
  withRecovery recover (Parser p) = Parser
    $ \s0 s e0 e pos hl inp -> p
      s0
      s
      (\err -> unParser (recover err pos)
        (\a _err' -> s0 a err)
        s
        (\_err' -> e0 err)
        (\_err' _pos' _hl' -> e0 err)
        pos
        hl
        inp)
      (\err pos' hl' -> unParser (recover err pos')
        (\a _err' -> s a err pos hl)
        s
        (\_err' -> e err pos' hl')
        (\_err' _pos'' _hl'' -> e err pos' hl')
        pos
        hl
        inp)
      pos
      hl
      inp

careted :: (SliceParsing m, Applicative m) => m a -> m (Position, a)
careted m = (,) <$> position <*> m

spanned :: (SliceParsing m, Applicative m) => m a -> m (Span, a)
spanned m = (\start a end -> (Span start end, a)) <$> position <*> m <*> position

-------------------------------------------------------------------------------
-- * Low-level queries
input :: Parser Text
input = Parser $ \s0 _s _e0 _e _pos _hl inp -> s0 inp mempty

highlights :: Parser Highlights
highlights = Parser $ \s0 _s _e0 _e _pos hl _inp -> s0 hl mempty

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
    $ slicedWith (\(a, s') b -> (f a b, s')) m

instance (SliceParsing m, MonadPlus m, Monoid w) => SliceParsing (Lazy.WriterT w m) where
  slicedWith f (Lazy.WriterT m)
    = Lazy.WriterT
    $ slicedWith (\(a, s') b -> (f a b, s')) m

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
  withRecovery r (Lazy.StateT m)
    = Lazy.StateT
    $ \s -> withRecovery (\err pos -> Lazy.runStateT (r err pos) s) (m s)

instance (RecoveryParsing m, MonadPlus m) => RecoveryParsing (Strict.StateT s m) where
  withRecovery r (Strict.StateT m)
    = Strict.StateT
    $ \s -> withRecovery (\err pos -> Strict.runStateT (r err pos) s) (m s)

instance (RecoveryParsing m, MonadPlus m) => RecoveryParsing (ReaderT e m) where
  withRecovery r (ReaderT m)
    = ReaderT
    $ \s -> withRecovery (\err pos -> runReaderT (r err pos) s) (m s)

instance (RecoveryParsing m, MonadPlus m, Monoid w) => RecoveryParsing (Strict.WriterT w m) where
  withRecovery r (Strict.WriterT m)
    = Strict.WriterT
    $ withRecovery (\err pos -> Strict.runWriterT $ r err pos) m

instance (RecoveryParsing m, MonadPlus m, Monoid w) => RecoveryParsing (Lazy.WriterT w m) where
  withRecovery r (Lazy.WriterT m)
    = Lazy.WriterT
    $ withRecovery (\err pos -> Lazy.runWriterT $ r err pos) m

instance (RecoveryParsing m, MonadPlus m, Monoid w) => RecoveryParsing (Lazy.RWST r w s m) where
  withRecovery r (Lazy.RWST m)
    = Lazy.RWST
    $ \s s' -> withRecovery (\err pos -> Lazy.runRWST (r err pos) s s') (m s s')

instance (RecoveryParsing m, MonadPlus m, Monoid w) => RecoveryParsing (Strict.RWST r w s m) where
  withRecovery r (Strict.RWST m)
    = Strict.RWST
    $ \s s' -> withRecovery (\err pos -> Strict.runRWST (r err pos) s s') (m s s')

instance (RecoveryParsing m, MonadPlus m) => RecoveryParsing (IdentityT m) where
  withRecovery r (IdentityT m)
    = IdentityT
    $ withRecovery (\err pos -> runIdentityT $ r err pos) m
