{-# LANGUAGE CPP #-}

module Internal.I where

#ifdef USE_INT64

import Data.Int (Int64)

type I = Int64

#else

import Data.Int (Int32)

type I = Int32

#endif
