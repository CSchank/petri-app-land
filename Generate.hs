module Generate where

import qualified Data.Map as M
import qualified Data.Set as S
import Types

generateClientState :: ClientServerApp -> [String]
generateClientState (clientState, _) =
	let 
		states = S.toList $ S.fromList $ map (fst . fst) $ M.toList clientState
	in
		map show states


generateCode :: ClientServerApp -> IO ()
generateCode scApp = do
	putStrLn "Hello"