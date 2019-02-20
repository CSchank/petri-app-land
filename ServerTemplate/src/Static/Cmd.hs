module Static.Cmd where

{-foldl :: (a -> b -> a) -> a -> [Cmd b] -> (b -> msg) -> Cmd msg
foldl fn initA lstCmdB toMsg = Cmd $ do-}

import Static.ServerTypes

foldl :: (a -> b -> a) -> a -> [Cmd b] -> (a -> msg) -> Cmd msg
foldl fn a cmdBs toMsg = CmdFold fn a cmdBs (return . toMsg)

none :: Cmd msg
none = CmdBatch []