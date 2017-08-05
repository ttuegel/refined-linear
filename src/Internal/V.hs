{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-@ LIQUID "--prune-unsorted" @-}

module Internal.V where

import Control.Monad.Primitive
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Storable

import Internal.I


{-@
data V a = V { vdim :: { n:I | n >= 0 }
             , vptr :: ForeignPtr a
             , voff :: { n:I | n >= 0 }
             , vinc :: { n:I | n /= 0 }
             }
@-}
data V a = V { vdim :: {-# UNPACK #-} !I
             , vptr :: {-# UNPACK #-} !(ForeignPtr a)
             , voff :: {-# UNPACK #-} !I
             , vinc :: {-# UNPACK #-} !I
             }

{-@
data MV s a = MV { mvdim :: { n:I | n >= 0 }
                 , mvptr :: ForeignPtr a
                 , mvoff :: { n:I | n >= 0 }
                 , mvinc :: { n:I | n /= 0 }
                 }
@-}
data MV s a = MV { mvdim :: {-# UNPACK #-} !I
                 , mvptr :: {-# UNPACK #-} !(ForeignPtr a)
                 , mvoff :: {-# UNPACK #-} !I
                 , mvinc :: {-# UNPACK #-} !I
                 }

{-@
slice :: Storable a =>
         { i:I | i >= 0 }
      -> { n:I | n >= 0 }
      -> { u:(V a) | i + n <= vdim u && i * (vinc u) + voff u >= 0 }
      -> { v:(V a) | n == vdim v }
@-}
slice :: Storable a => I -> I -> V a -> V a
slice i n u =
  V { vdim = n
    , vptr = vptr u
    , voff = voff u + i * (vinc u)
    , vinc = vinc u
    }

{-@
new :: (PrimMonad m, Storable a) =>
       { n:I | n >= 0 } -> m { v:(MV (PrimState m) a) | mvdim v == n }
@-}
new :: (PrimMonad m, Storable a) => I -> m (MV (PrimState m) a)
new mvdim = unsafePrimToPrim $ do
  mvptr <- mallocForeignPtrArray (fromIntegral mvdim)
  let
    mvinc = 1
    mvoff = 0
  pure MV {..}

{-@
indexPrim :: (PrimMonad m, Storable a) =>
             v:(V a) -> { i:I | i >= 0 && i < vdim v } -> m a
@-}
indexPrim :: (PrimMonad m, Storable a) => V a -> I -> m a
indexPrim (V {..}) i =
  unsafePrimToPrim $
  withForeignPtr vptr $ \ptr ->
  peekElemOff ptr (fromIntegral (voff + i * vinc))

{-@
indexM :: (Monad m, Storable a) =>
          v:(V a) -> { i:I | i >= 0 && i < vdim v } -> m a
@-}
indexM :: (PrimMonad m, Storable a) => V a -> I -> m a
indexM (V {..}) i =
  (pure . unsafeInlineIO) $
  withForeignPtr vptr $ \ptr ->
  peekElemOff ptr (fromIntegral (voff + i * vinc))

{-@
read :: (PrimMonad m, Storable a) =>
        v:(MV (PrimState m) a) -> { i:I | i >= 0 && i < mvdim v } -> m a
@-}
read :: (PrimMonad m, Storable a) => MV (PrimState m) a -> I -> m a
read (MV {..}) i =
  unsafePrimToPrim $
  withForeignPtr mvptr $ \ptr ->
  peekElemOff ptr (fromIntegral (mvoff + i * mvinc))

{-@
write :: (PrimMonad m, Storable a) =>
         v:(MV (PrimState m) a) -> { i:I | i >= 0 && i < mvdim v } -> a -> m ()
@-}
write :: (PrimMonad m, Storable a) => MV (PrimState m) a -> I -> a -> m ()
write (MV {..}) i a =
  unsafePrimToPrim $
  withForeignPtr mvptr $ \ptr ->
  pokeElemOff ptr (fromIntegral (mvoff + i * mvinc)) a

{-@
unsafeFreeze :: (PrimMonad m, Storable a) =>
                mv:(MV (PrimState m) a) -> m { v:(V a) | mvdim mv == vdim v }
@-}
unsafeFreeze :: (PrimMonad m, Storable a) => MV (PrimState m) a -> m (V a)
unsafeFreeze (MV {..}) = do
  pure V { vdim = mvdim
         , vptr = mvptr
         , voff = mvoff
         , vinc = mvinc
         }

{-@
unsafeThaw :: (PrimMonad m, Storable a) =>
              v:(V a) -> m { mv:(MV (PrimState m) a) | mvdim mv == vdim v }
@-}
unsafeThaw :: (PrimMonad m, Storable a) => V a -> m (MV (PrimState m) a)
unsafeThaw (V {..}) = do
  pure MV { mvdim = vdim
          , mvptr = vptr
          , mvoff = voff
          , mvinc = vinc
          }

{-@
copy :: (PrimMonad m, Storable a) =>
        mv:(MV (PrimState m) a)
     -> { v:(V a) | vdim v == mvdim mv } -> m ()
@-}
copy :: (PrimMonad m, Storable a) => MV (PrimState m) a -> V a -> m ()
copy dst src = do
  let
    copy_go i = do
          indexPrim src i >>= write dst i
          if i > 0 then copy_go (i - 1) else pure ()
    beg = vdim src - 1
  if beg >= 0 then copy_go beg else pure ()

{-@
freeze :: (PrimMonad m, Storable a) =>
          mv:(MV (PrimState m) a) -> m { v:(V a) | vdim v == mvdim mv }
@-}
freeze :: (PrimMonad m, Storable a) => MV (PrimState m) a -> m (V a)
freeze mv = do
  v <- new (mvdim mv)
  copy v =<< unsafeFreeze mv
  unsafeFreeze v

{-@
thaw :: (PrimMonad m, Storable a) =>
        v:(V a) -> m { mv:(MV (PrimState m) a) | vdim v == mvdim mv }
@-}
thaw :: (PrimMonad m, Storable a) => V a -> m (MV (PrimState m) a)
thaw v = do
  mv <- new (vdim v)
  copy mv v
  pure mv
