module Plugins.Files where

import Static.ServerTypes
import Utils.Utils
import System.IO.Error
import Control.Exception
import Static.Result (Result(..))
import Static.Cmd (Cmd(..))

data Error =
      DoesNotExist
    | AlreadyExists
    | AlreadyInUse
    | DeviceFull
    | EOFError
    | PermissionError
    | OtherError String

exceptionHandler :: IOError -> IO (Result Error a)
exceptionHandler e
    | isAlreadyExistsError e = return $ Err AlreadyExists
    | isDoesNotExistError e = return $ Err DoesNotExist
    | isAlreadyInUseError e = return $ Err AlreadyInUse
    | isFullError e = return $ Err DeviceFull
    | isEOFError e = return $ Err EOFError
    | isPermissionError e = return $ Err PermissionError
    | otherwise = return $ Err $ OtherError $ show e

doReadFile :: FilePath -> (Result Error String -> msg) -> Cmd msg
doReadFile fp msg = Cmd $ fmap msg $ do
    (fmap Ok $ readFile fp) `catch` exceptionHandler

doWriteFile :: FilePath -> String -> (Result Error () -> msg) -> Cmd msg
doWriteFile fp contents msg = Cmd $ fmap msg $ do
    (fmap Ok $ writeFile contents fp) `catch` exceptionHandler
