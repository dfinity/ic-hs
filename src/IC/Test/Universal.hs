{- |

Helpers to program the “Universal module”. This is essentially a small,
type-safe DSL to produce the small stack-based programming language interpreted
by the universal canister.

This DSL is expression-based, not stack based; seems to suite all our needs and is
simpler to work with.

This language is not stable; therefore there is no separarte documentation of
specification than this file and `impl/universal-canister/src/`
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IC.Test.Universal where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Builder
import Data.Word
import Data.String

-- The types of our little language are i32 and blobs

data T = I | B


-- We deal with expressions (return a value, thus have a type) and programs (do
-- something, but do not return a type). They are represented simply
-- by the encoded stack program; no need for an AST or something that complicated

newtype Exp (result :: T) where
    Exp :: Builder -> Exp a

newtype Prog where
    Prog :: Builder -> Prog

-- We extracting the actual stack program bytecode from a program.

run :: Prog -> BS.ByteString
run (Prog x) = toLazyByteString x

-- Programs can be sequenced using (>>>); this naturally forms a Monoid

(>>>) :: Prog -> Prog -> Prog
Prog a >>> Prog b = Prog (a <> b)

instance Semigroup Prog where
    (<>) = (>>>)

instance Monoid Prog where
    mempty = Prog mempty

-- A utility class to easily defined functions and programs of any arity
-- simply by specifying their type.

class Op a where
    mkOp :: Word8 -> Builder -> a

instance Op Prog
    where mkOp x args = Prog $ args <> word8 x
instance Op (Exp t)
    where mkOp x args = Exp $ args <> word8 x
instance Op a => Op (Exp t -> a)
    where mkOp x args (Exp a) = mkOp x (args <> a)

op :: Op a => Word8 -> a
op x = mkOp x mempty

-- Now, all the op codes defined by the universal canister.
-- Most can be simply be defined by specifiying their type and using the 'op'
-- combinator

noop :: Prog
noop = op 0

ignore :: Exp t -> Prog
ignore = op 1

int :: Word32 -> Exp 'I
int x = Exp $ word8 2 <> word32LE x

bytes :: BS.ByteString -> Exp 'B
bytes bytes = Exp $
    word8 3 <>
    word32LE (fromIntegral (BS.length bytes)) <>
    lazyByteString bytes

replyDataAppend :: Exp 'B -> Prog
replyDataAppend = op 4

reply :: Prog
reply = op 5

self :: Exp 'B
self = op 6

reject :: Exp 'B -> Prog
reject = op 7

caller :: Exp 'B
caller = op 8

call_simple :: Exp 'B -> Exp 'B -> Exp 'B -> Exp 'B -> Exp 'B -> Prog
call_simple = op 9

reject_msg :: Exp 'B
reject_msg = op 10

reject_code :: Exp 'I
reject_code = op 11

i2b :: Exp 'I -> Exp 'B
i2b = op 12

argData :: Exp 'B
argData = op 13

cat :: Exp 'B -> Exp 'B -> Exp 'B
cat = op 14

stableSize :: Exp 'I
stableSize = op 15

stableGrow :: Exp 'I -> Exp 'I
stableGrow = op 16

stableRead :: Exp 'I -> Exp 'I -> Exp 'B
stableRead = op 17

stableWrite :: Exp 'I -> Exp 'B -> Prog
stableWrite = op 18

debugPrint :: Exp 'B -> Prog
debugPrint = op 19

trap :: Exp 'B -> Prog
trap = op 20

setGlobal :: Exp 'B -> Prog
setGlobal = op 21

getGlobal :: Exp 'B
getGlobal = op 22

badPrint :: Prog
badPrint = op 23

onPreUpgrade :: Exp 'B -> Prog
onPreUpgrade = op 24


-- Some convenience combinators

-- This allows us to write byte expressions as plain string literals
instance IsString (Exp 'B) where
  fromString s = bytes (fromString s)

callback :: Prog -> Exp 'B
callback = bytes . run

replyData :: Exp 'B -> Prog
replyData a = replyDataAppend a >>> reply

-- Convenient inter-canister calling

data CallArgs = CallArgs
    { on_reply :: Prog
    , on_reject :: Prog
    , other_side :: Prog
    }

inter_call :: BS.ByteString -> BS.ByteString -> CallArgs -> Prog
inter_call callee method_name ca = call_simple
    (bytes callee)
    (bytes method_name)
    (callback (on_reply ca))
    (callback (on_reject ca))
    (callback (other_side ca))

inter_update :: BS.ByteString -> CallArgs -> Prog
inter_update callee = inter_call callee "update"

inter_query :: BS.ByteString -> CallArgs -> Prog
inter_query callee = inter_call callee "query"

-- | By default, the other side responds with some text
-- indicating caller and callee, and the callbacks reply with the response.
defArgs :: CallArgs
defArgs = CallArgs
    { on_reply = replyArgData
    , on_reject = replyRejectData
    , other_side = defaulOtherSide
    }

defaulOtherSide :: Prog
defaulOtherSide =
    replyDataAppend "Hello " >>>
    replyDataAppend caller  >>>
    replyDataAppend " this is " >>>
    replyDataAppend self  >>>
    reply

replyArgData :: Prog
replyArgData = replyData argData

replyRejectData :: Prog
replyRejectData =
    replyDataAppend (i2b reject_code) >>>
    replyDataAppend reject_msg >>>
    reply

