{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Plugins.Users where

import Static.ServerTypes
import Crypto.BCrypt
import Data.Typeable (Typeable)
import Data.Acid
import Data.Acid.Advanced   (update', query')
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Data.Map.Strict as M'
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import Data.Maybe (fromJust)

import qualified Data.Set as Set

import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)

data Group =
      Administrator
    | Supervisor
    | User
    | Custom String
    deriving (Eq,Show,Ord)
$(deriveSafeCopy 0 'base ''Group)

data UserData = 
    UserData 
        {
            pwdHash :: ByteString
        ,   groups :: Set.Set Group
        }
$(deriveSafeCopy 0 'base ''UserData)

newtype UserDict = UserDict (M'.Map T.Text UserData)

$(deriveSafeCopy 0 'base ''UserDict)


data Users =
    Users 
    {
        userDb :: AcidState UserDict
    }

instance (Plugin Users) where
    initPlugin = do
        putStrLn "Loading users database...."
        as <- openLocalState (UserDict M'.empty)
        return $ 
            Users
                {
                    userDb = as
                }

lookupPasswordByName :: T.Text -> Query UserDict ByteString
lookupPasswordByName username = do
    UserDict userDb <- ask
    case M'.lookup username userDb of
        Just b -> return $ pwdHash b
        Nothing -> return ""

addUser :: T.Text -> UserData -> Update UserDict Bool
addUser username dat = do
    UserDict userDb <- get
    case M'.lookup username userDb of
        Just _ -> return False
        Nothing -> do
            put $ UserDict $ M'.insert username dat userDb
            return True

removeUser :: T.Text -> Update UserDict Bool
removeUser username = do
    UserDict userDb <- get
    case M'.lookup username userDb of
        Just _ -> do
            put $ UserDict $ M'.delete username userDb
            return True
        Nothing -> 
            return False

addGroup :: T.Text -> Group -> Update UserDict Bool
addGroup username group = do
    UserDict userDb <- get
    case M'.lookup username userDb of
        Just dat -> do
            put $ UserDict $ M'.insert username (dat { groups = Set.insert group $ groups dat }) userDb
            return True
        Nothing -> 
            return False

removeGroup :: T.Text -> Group -> Update UserDict Bool
removeGroup username group = do
    UserDict userDb <- get
    case M'.lookup username userDb of
        Just dat -> do
            put $ UserDict $ M'.insert username (dat { groups = Set.delete group $ groups dat }) userDb
            return True
        Nothing -> 
            return False


changePwd :: T.Text -> ByteString -> Update UserDict Bool
changePwd username newPwd = do
    UserDict userDb <- get
    case M'.lookup username userDb of
        Just dat -> do
            put $ UserDict $ M'.insert username (dat { pwdHash = newPwd }) userDb
            return True
        Nothing -> 
            return False


$(makeAcidic ''UserDict [ 'lookupPasswordByName
                        , 'addUser
                        , 'removeUser
                        , 'addGroup
                        , 'removeGroup
                        , 'changePwd
                      ])

validateUser :: String -> String -> (Bool -> msg) -> Cmd msg
validateUser username password msgf = StateCmd $ \users -> do
    let uDb = userDb users
    pHash <- query' uDb (LookupPasswordByName $ T.pack username)
    return $ msgf $ validatePassword pHash (pack password)

currentHashingScheme = slowerBcryptHashingPolicy

insertUser :: String -> String -> [Group] -> (Bool -> msg) -> Cmd msg
insertUser username password groups msgf = StateCmd $ \users -> do
    let uDb = userDb users
    newPwdHash <- fmap fromJust $ hashPasswordUsingPolicy currentHashingScheme (pack password)
    let newUserData = UserData { groups = Set.fromList groups, pwdHash = newPwdHash }
    res <- update' uDb (AddUser (T.pack username) newUserData)
    return $ msgf res

deleteUser :: String -> (Bool -> msg) -> Cmd msg
deleteUser username msgf = StateCmd $ \users -> do
    let uDb = userDb users
    res <- update' uDb (RemoveUser $ T.pack username)
    return $ msgf $ res

assignGroup :: String -> Group -> (Bool -> msg) -> Cmd msg
assignGroup username group msgf = StateCmd $ \users -> do
    let uDb = userDb users
    res <- update' uDb (AddGroup (T.pack username) group)
    return $ msgf res

revokeGroup :: String -> Group -> (Bool -> msg) -> Cmd msg
revokeGroup username group msgf = StateCmd $ \users -> do
    let uDb = userDb users
    res <- update' uDb (RemoveGroup (T.pack username) group)
    return $ msgf res

updatePwd :: String -> String -> (Bool -> msg) -> Cmd msg
updatePwd username password msgf = StateCmd $ \users -> do
    let uDb = userDb users
    newPwdHash <- fmap fromJust $ hashPasswordUsingPolicy currentHashingScheme (pack password)
    res <- update' uDb (ChangePwd (T.pack username) newPwdHash)
    return $ msgf res