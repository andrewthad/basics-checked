{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.ShortText
  ( -- Types
    T
  , T#
  , R
    -- Lifting
  , lift
  , unlift
    -- Array
  , read#
  , write#
  , index#
  , uninitialized#
  , initialized#
  , copy#
  , copyMutable#
  , set#
  , shrink#
  , eq#
  , neq#
    -- Encode
  , shows
  ) where

import Prelude hiding (shows)

import GHC.Exts hiding (setByteArray#)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Text.Short (ShortText)

import qualified Prelude
import qualified GHC.Exts as Exts
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS

type T = ShortText
type T# = ByteArray#
type R = 'BoxedRep 'Unlifted

lift :: T# -> T
{-# inline lift #-}
lift x = TS.fromShortByteStringUnsafe (SBS x)

unlift :: T -> T#
{-# inline unlift #-}
unlift t = case TS.toShortByteString t of { SBS x -> x }

index# :: ArrayArray# -> Int# -> T#
{-# noinline index# #-}
index# xs i = case i <# 0# of
  1# -> error ("Basics.ShortText.index#: negative index " ++ show (I# i))
  _ -> case i >=# Exts.sizeofArrayArray# xs of
    1# -> error ("Basics.ShortText.index#: index " ++ show (I# i) ++ " >= length " ++ show (I# (sizeofArrayArray# xs)))
    _ -> Exts.indexByteArrayArray# xs i

read# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# arr i st = case i <# 0# of
  1# -> error ("Basics.ShortText.read#: negative index " ++ show (I# i))
  _ ->
    let sz = Exts.sizeofMutableArrayArray# arr in
    case i >=# sz of
      1# -> error ("Basics.ShortText.read#: index " ++ show (I# i) ++ " >= length " ++ show (I# sz))
      _ -> Exts.readByteArrayArray# arr i st

write# :: MutableArrayArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# arr i v st = case i <# 0# of
  1# -> error ("Basics.ShortText.write#: negative index " ++ show (I# i))
  _ ->
    let sz = Exts.sizeofMutableArrayArray# arr in
    case i >=# sz of
      1# -> error ("Basics.ShortText.write#: index " ++ show (I# i) ++ " >= length " ++ show (I# sz))
      _ -> Exts.writeByteArrayArray# arr i v st

set# :: MutableArrayArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

-- | In the unchecked library, this is very unsafe. Here in the checked
-- library, we go ahead and initialize all elements to the empty ArrayArray#.
uninitialized# :: Int# -> State# s -> (# State# s, MutableArrayArray# s #)
{-# noinline uninitialized# #-}
uninitialized# n s0 = case Exts.newByteArray# 0# s0 of
  (# s1, e #) -> case Exts.unsafeFreezeByteArray# e s1 of
    (# s2, e' #) -> initialized# n e' s2

initialized# ::
     Int# -> T# -> State# s
  -> (# State# s, MutableArrayArray# s #)
{-# inline initialized# #-}
initialized# n e s0 = case Exts.newArrayArray# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

copy# :: MutableArrayArray# s -> Int# -> ArrayArray# -> Int# -> Int# -> State# s -> State# s
{-# inline copy# #-}
copy# dst doff src soff len =
  Exts.copyArrayArray# src soff dst doff len

copyMutable# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len =
  Exts.copyMutableArrayArray# src soff dst doff len

shrink# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableArrayArray# s #)
{-# inline shrink# #-}
shrink# m sz s0 = case uninitialized# sz s0 of
  (# s1, dst #) -> case copyMutable# dst 0# m 0# sz s1 of
    s2 -> (# s2, dst #)

eq# :: ByteArray# -> ByteArray# -> Int#
{-# inline eq# #-}
eq# a b = case lenA ==# lenB of
  1# -> Exts.compareByteArrays# a 0# b 0# lenA ==# 0#
  _ -> 0#
  where
  !lenA = Exts.sizeofByteArray# a
  !lenB = Exts.sizeofByteArray# b

neq# :: ByteArray# -> ByteArray# -> Int#
{-# inline neq# #-}
neq# a b = case lenA ==# lenB of
  1# -> Exts.compareByteArrays# a 0# b 0# lenA /=# 0#
  _ -> 1#
  where
  !lenA = Exts.sizeofByteArray# a
  !lenB = Exts.sizeofByteArray# b

shows :: T -> String -> String
shows = Prelude.shows
