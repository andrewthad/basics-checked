{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Int64
  ( -- Types
    T
  , T#
  , R
    -- Lifting
  , lift
  , unlift
    -- Arithmetic
  , plus
  , minus
  , plus#
  , minus#
  , times#
  , quot#
  , rem#
    -- Compare
  , gt#
  , lt#
  , gte#
  , lte#
  , eq#
  , neq#
  , gt
  , lt
  , gte
  , lte
  , eq
  , neq
    -- Array
  , read#
  , write#
  , index#
  , read
  , write
  , index
  , uninitialized#
  , initialized#
  , uninitialized
  , initialized
  , copy#
  , copyMutable#
  , set#
  , shrink#
  , shrink
    -- Constants
  , zero
  , def
  , minBound
  , maxBound
  , infimum
  , supremum
    -- Metadata
  , signed
    -- Encoding
  , shows
  ) where

import Prelude hiding (shows,maxBound,minBound,read)

import GHC.Exts
import GHC.Int
import Data.Primitive (MutableByteArray(..),ByteArray(..))
import GHC.ST (ST(ST))

import qualified Prelude
import qualified GHC.Exts as Exts

type T = Int64
type T# = Int#
type R = 'IntRep

def :: T
{-# inline def #-}
def = 0

zero :: T
{-# inline zero #-}
zero = 0

minBound :: T
{-# inline minBound #-}
minBound = I64# (-9223372036854775808#)

maxBound :: T
{-# inline maxBound #-}
maxBound = I64# (9223372036854775807#)

infimum :: T
{-# inline infimum #-}
infimum = I64# (-9223372036854775808#)

supremum :: T
{-# inline supremum #-}
supremum = I64# (9223372036854775807#)

signed :: Bool
{-# inline signed #-}
signed = True

lift :: T# -> T
{-# inline lift #-}
lift = I64#

unlift :: T -> T#
{-# inline unlift #-}
unlift (I64# i) = i

plus :: T -> T -> T
{-# inline plus #-}
plus (I64# x) (I64# y) = I64# (x +# y)

minus :: T -> T -> T
{-# inline minus #-}
minus (I64# x) (I64# y) = I64# (x -# y)

times# :: T# -> T# -> T#
{-# inline times# #-}
times# = (*#)

quot# :: T# -> T# -> T#
{-# inline quot# #-}
quot# = quotInt#

rem# :: T# -> T# -> T#
{-# inline rem# #-}
rem# = remInt#

plus# :: T# -> T# -> T#
{-# inline plus# #-}
plus# = (+#)

minus# :: T# -> T# -> T#
{-# inline minus# #-}
minus# = (-#)

gt# :: T# -> T# -> Int#
{-# inline gt# #-}
gt# = (>#)

lt# :: T# -> T# -> Int#
{-# inline lt# #-}
lt# = (<#)

gte# :: T# -> T# -> Int#
{-# inline gte# #-}
gte# = (>=#)

lte# :: T# -> T# -> Int#
{-# inline lte# #-}
lte# = (<=#)

eq# :: T# -> T# -> Int#
{-# inline eq# #-}
eq# = (==#)

neq# :: T# -> T# -> Int#
{-# inline neq# #-}
neq# = (/=#)

gt :: T -> T -> Bool
{-# inline gt #-}
gt = (>)

lt :: T -> T -> Bool
{-# inline lt #-}
lt = (<)

gte :: T -> T -> Bool
{-# inline gte #-}
gte = (>=)

lte :: T -> T -> Bool
{-# inline lte #-}
lte = (<=)

eq :: T -> T -> Bool
{-# inline eq #-}
eq = (==)

neq :: T -> T -> Bool
{-# inline neq #-}
neq = (/=)

index# :: ByteArray# -> Int# -> T#
{-# noinline index# #-}
index# xs i = case i <# 0# of
  1# -> error ("Basics.Int64.index#: negative index " ++ show (I# i))
  _ -> case remInt# sz 8# of
    0# -> case i >=# quotInt# sz 8# of
      1# -> error ("Basics.Int64.index#: index " ++ show (I# i) ++ " >= length " ++ show (I# (quotInt# sz 8#)))
      _ -> indexIntArray# xs i
    _ -> error "Basics.Int64.index#: array size did not divide 8 evenly"
  where
  sz = sizeofByteArray# xs

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# arr i st = case i <# 0# of
  1# -> error ("Basics.Int64.read#: negative index " ++ show (I# i))
  _ -> case getSizeofMutableByteArray# arr st of
    (# st', sz #) -> case remInt# sz 8# of
      0# -> case i >=# quotInt# sz 8# of
        1# -> error ("Basics.Int64.read#: index " ++ show (I# i) ++ " >= length " ++ show (I# (quotInt# sz 8#)))
        _ -> readIntArray# arr i st'
      _ -> error "Basics.Int64.read#: array size did not divide 8 evenly"

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# arr i e st = case i <# 0# of
  1# -> error ("Basics.Int64.write#: negative index " ++ show (I# i))
  _ -> case getSizeofMutableByteArray# arr st of
    (# st', sz #) -> case remInt# sz 8# of
      0# -> case i >=# quotInt# sz 8# of
        1# -> error ("Basics.Int64.write#: index " ++ show (I# i) ++ " >= length " ++ show (I# (quotInt# sz 8#)))
        _ -> writeIntArray# arr i e st'
      _ -> error "Basics.Int64.write#: array size did not divide 8 evenly"

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline uninitialized# #-}
uninitialized# sz = Exts.newByteArray# (sz *# 8# )

initialized# ::
     Int# -> T# -> State# s
  -> (# State# s, MutableByteArray# s #)
{-# inline initialized# #-}
initialized# n e s0 = case uninitialized# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

uninitialized :: Int -> ST s (MutableByteArray s)
{-# inline uninitialized #-}
uninitialized (I# sz) = ST $ \s0 -> case uninitialized# sz s0 of
  (# s1, a #) -> (# s1, MutableByteArray a #)

initialized :: Int -> T -> ST s (MutableByteArray s)
{-# inline initialized #-}
initialized (I# sz) e = ST $ \s0 -> case initialized# sz (unlift e) s0 of
  (# s1, a #) -> (# s1, MutableByteArray a #)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline shrink# #-}
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# 8#) s0, m #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
{-# inline copy# #-}
copy# dst doff src soff len =
  Exts.copyByteArray# src (soff *# 8#) dst (doff *# 8#) (len *# 8#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 8#) dst (doff *# 8#) (len *# 8#)

shows :: T -> String -> String
{-# inline shows #-}
shows = Prelude.shows

index :: ByteArray -> Int -> T
{-# inline index #-}
index (ByteArray x) (I# i) = I64# (indexInt64Array# x i)

read :: MutableByteArray s -> Int -> ST s T
{-# inline read #-}
read (MutableByteArray x) (I# i) = ST
  (\s0 -> case readInt64Array# x i s0 of
    (# s1, r #) -> (# s1, I64# r #)
  )

write :: MutableByteArray s -> Int -> T -> ST s ()
{-# inline write #-}
write (MutableByteArray x) (I# i) (I64# e) = ST (\s -> (# writeInt64Array# x i e s, () #) )

shrink :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
{-# inline shrink #-}
shrink (MutableByteArray x) (I# i) = ST
  (\s0 -> case shrink# x i s0 of
    (# s1, r #) -> (# s1, MutableByteArray r #)
  )
