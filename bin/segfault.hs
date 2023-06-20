{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc (malloc, mallocBytes)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.Storable
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

data C'wasm_engine_t = C'wasm_engine_t

data C'wasmtime_store_t = C'wasmtime_store_t

data C'wasmtime_context_t = C'wasmtime_context_t

data C'wasmtime_module_t = C'wasmtime_module_t

data C'wasmtime_error_t = C'wasmtime_error_t

data C'wasm_trap_t = C'wasm_trap_t

data C'wasmtime_extern_t = C'wasmtime_extern_t

type C'wasmtime_trap_code_t = Word8

data C'wasmtime_instance = C'wasmtime_instance
  { c'wasmtime_instance'store_id :: Word64,
    c'wasmtime_instance'index :: CSize
  }
  deriving (Eq, Show)

instance Storable C'wasmtime_instance where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'wasmtime_instance v0 v1
  poke _p (C'wasmtime_instance v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

type C'wasmtime_instance_t = C'wasmtime_instance

foreign import ccall unsafe "wasm_engine_new"
  c'wasm_engine_new ::
    IO (Ptr C'wasm_engine_t)

foreign import ccall unsafe "wasmtime_store_new"
  c'wasmtime_store_new ::
    Ptr C'wasm_engine_t -> Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (Ptr C'wasmtime_store_t)

foreign import ccall unsafe "wasmtime_store_context"
  c'wasmtime_store_context ::
    Ptr C'wasmtime_store_t -> IO (Ptr C'wasmtime_context_t)

foreign import ccall unsafe "wasmtime_module_new"
  c'wasmtime_module_new ::
    Ptr C'wasm_engine_t -> Ptr Word8 -> CSize -> Ptr (Ptr C'wasmtime_module_t) -> IO (Ptr C'wasmtime_error_t)

foreign import ccall unsafe "wasmtime_instance_new"
  c'wasmtime_instance_new ::
    Ptr C'wasmtime_context_t -> Ptr C'wasmtime_module_t -> Ptr C'wasmtime_extern_t -> CSize -> Ptr C'wasmtime_instance_t -> Ptr (Ptr C'wasm_trap_t) -> IO (Ptr C'wasmtime_error_t)

foreign import ccall unsafe "wasmtime_trap_code"
  c'wasmtime_trap_code ::
    Ptr C'wasm_trap_t -> Ptr C'wasmtime_trap_code_t -> IO Bool

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  engine_ptr <- c'wasm_engine_new
  wasmtime_store_ptr <- c'wasmtime_store_new engine_ptr nullPtr nullFunPtr
  ctx_ptr <- c'wasmtime_store_context wasmtime_store_ptr
  let wasm = [0, 97, 115, 109, 1, 0, 0, 0] :: [Word8]
      -- wasm = [0, 97, 115, 109, 1, 0, 0, 0, 1, 6, 1, 96, 1, 127, 1, 127, 3, 2, 1, 0, 7, 13, 1, 9, 102, 105, 98, 111, 110, 97, 99, 99, 105, 0, 0, 10, 30, 1, 28, 0, 32, 0, 65, 2, 72, 4, 64, 32, 0, 15, 11, 32, 0, 65, 1, 107, 16, 0, 32, 0, 65, 2, 107, 16, 0, 106, 11] :: [Word8]
      n = length wasm
  p <- mallocBytes n
  pokeArray p wasm

  module_ptr_ptr <- malloc
  error_ptr <-
    c'wasmtime_module_new
      engine_ptr
      p
      (fromIntegral n)
      module_ptr_ptr

  print ("c'wasmtime_module_new: error_ptr", error_ptr)

  mod_ptr <- peek module_ptr_ptr

  putStrLn "Creating Instance ..."
  (inst_ptr :: Ptr C'wasmtime_instance_t) <- malloc
  (trap_ptr_ptr :: Ptr (Ptr C'wasm_trap_t)) <- malloc

  error_ptr <-
    c'wasmtime_instance_new
      ctx_ptr
      mod_ptr
      nullPtr
      0
      inst_ptr
      trap_ptr_ptr

  print ("c'wasmtime_instance_new: error_ptr", error_ptr)

  putStrLn "done"

  trap_ptr <- peek trap_ptr_ptr

  putStrLn "after peek"

  if trap_ptr == nullPtr
    then do
      error "TODO"
    else do
      putStrLn "Received trap:"
      print trap_ptr

      code_ptr <- malloc
      isInstructionTrap <- c'wasmtime_trap_code trap_ptr code_ptr
      if isInstructionTrap
        then do
          code <- peek code_ptr
          print ("code", code)
        else putStrLn "No instructrion trap"

      error "TODO"

  putStrLn "The impossible happened!"

  error "another error!"
