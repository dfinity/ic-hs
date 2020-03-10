module IC.Logger where

import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Except

class Logger m where
    logTrap :: String -> m ()

instance (Monad m, Logger m) => Logger (StateT s m) where
    logTrap x = lift $ logTrap x

instance (Monad m, Logger m) => Logger (ContT s m) where
    logTrap x = lift $ logTrap x

instance (Monad m, Logger m) => Logger (ExceptT s m) where
    logTrap x = lift $ logTrap x


instance Logger IO where
    logTrap msg = putStrLn $ "Trap: " ++ msg
