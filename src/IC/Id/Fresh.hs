module IC.Id.Fresh where

import IC.Types
import IC.Id.Forms

import Data.Binary (encode)
import Data.Word

-- Not particulary efficent, but this is a reference implementation, right?
freshId :: [EntityId] -> EntityId
freshId ids =
    head $
    filter (`notElem` ids) $
    map (EntityId . mkOpaqueId . encode)
    [1024::Word64 ..]
