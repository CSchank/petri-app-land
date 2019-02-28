{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveDataTypeable, TypeSynonymInstances #-}

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

import Data.Data            (Data, Typeable)
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..), getOne
                            , ixFun, ixSet )
import qualified Data.IxSet as IX

import Data.Word (Word32)

import Static.Task as Task
import Plugins.Users.Types
import Static.Result as Result (Result(..),map)
import qualified Data.IntMap as IM

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, modifyTVar, readTVar)

import Utils.Utils ((|>))

type Users = Plugins.Users.Types.Users

currentHashingScheme = slowerBcryptHashingPolicy
    {
        preferredHashCost = 10
    }

instance (Plugin Plugins.Users.Users) where
    initPlugin = do
        putStrLn "Loading users database...."
        pwd <- fmap fromJust $ hashPasswordUsingPolicy currentHashingScheme (pack "Pass1")
        let newUser = UserData
                {
                    userID = UserID 0
                ,   username = Username "Chris"
                ,   pwdHash = Password pwd
                ,   groups = Groups $ Set.fromList [Administrator]
                }
        let newUser1 = UserData
                {
                    userID = UserID 1
                ,   username = Username "OtherChris"
                ,   pwdHash = Password pwd
                ,   groups = Groups Set.empty
                }
        let initialDB = UserDB
                {
                    users = IX.insert newUser $ IX.insert newUser1 IX.empty
                ,   nextUserID = 2
                }
        as <- openLocalState initialDB
        newTv <- atomically $ newTVar (IM.empty)
        return $ 
            Users
                {
                    userDb = as
                ,   sessions = newTv
                }

lookupPasswordByName :: T.Text -> Query UserDB (Maybe ByteString)
lookupPasswordByName username = do
    ut <- ask
    let uDb = users ut
    case IX.getOne $ IX.getEQ (Username username) uDb of
        Just head -> do
            let (Password pwh) = pwdHash head
            return $ Just pwh
        _ -> 
            return Nothing


lookupGroupsByID :: Word32 -> Query UserDB (Result Error (Set.Set Group))
lookupGroupsByID userID = do
    ut <- ask
    let uDb = users ut
    case IX.getOne $ IX.getEQ (UserID userID) uDb of
        Just head -> do
            let (Groups gr) = groups head
            return $ Ok gr
        _ -> 
            return $ Err UserNotFound

lookupIDByUsername :: T.Text -> Query UserDB (Result Error Word32)
lookupIDByUsername username = do
    ut <- ask
    let uDb = users ut
    case IX.getOne $ IX.getEQ (Username username) uDb of
        Just head -> do
            let (UserID uId) = userID head
            return $ Ok uId
        _ -> 
            return $ Err UserNotFound

addUser :: T.Text -> UserData -> Update UserDB (Result Error Word32)
addUser username dat = do
    ut <- get
    let uDb = users ut
        newUserId = nextUserID ut
    case IX.toList $ IX.getEQ (Username username) uDb of
        [] -> do
            put $ ut 
                { 
                    users = IX.insert (dat { userID = UserID newUserId }) uDb
                ,   nextUserID = newUserId + 1
                }
            return $ Ok newUserId
        _ -> 
            return $ Err $ UserNotFound

removeUser :: T.Text -> Update UserDB (Result Error ())
removeUser username = do
    ut <- get
    let uDb = users ut
    case IX.getOne $ IX.getEQ (Username username) uDb of
        Just head -> do
            put $ ut { 
                        users = IX.deleteIx 
                                (Username username) 
                                uDb
                        }
            return $ Ok ()
        _ -> 
            return $ Err UserNotFound

addGroup :: T.Text -> Group -> Update UserDB (Result Error ())
addGroup username group = do
    ut <- get
    let uDb = users ut
    case IX.getOne $ IX.getEQ (Username username) uDb of
        Just head -> do
            let (Groups userGroups) = groups head
            put $ ut { 
                        users = IX.updateIx 
                                (Username username) 
                                (head { groups = Groups $ Set.insert group $ userGroups })
                                uDb
                        }
            return $ Ok ()
        _ -> 
            return $ Err UserNotFound

removeGroup :: T.Text -> Group -> Update UserDB (Result Error ())
removeGroup username group = do
    ut <- get
    let uDb = users ut
    case IX.getOne $ IX.getEQ (Username username) uDb of
        Just head -> do
            let (Groups userGroups) = groups head
            put $ ut { 
                        users = IX.updateIx 
                                (Username username) 
                                (head { groups = Groups $ Set.delete group $ userGroups })
                                uDb
                        }
            return $ Ok ()
        _ -> 
            return $ Err UserNotFound


changePwd :: T.Text -> ByteString -> Update UserDB (Result Error ())
changePwd username newPwd = do
    ut <- get
    let uDb = users ut
    case IX.getOne $ IX.getEQ (Username username) uDb of
        Just head -> do
            put $ ut { 
                        users = IX.updateIx 
                                (Username username) 
                                (head { pwdHash = Password newPwd }) 
                                uDb
                        }
            return $ Ok ()
        _ -> 
            return $ Err UserNotFound


changeUsernameByID :: Word32 -> T.Text -> Update UserDB (Result Error ())
changeUsernameByID userID newUsername = do
    ut <- get
    let uDb = users ut
    case IX.getOne $ IX.getEQ (UserID $ fromIntegral userID) uDb of
        Just head -> do
            put $ ut { 
                        users = IX.updateIx 
                                (UserID userID) 
                                (head { username = Username newUsername }) 
                                uDb
                        }
            return $ Ok ()
        _ -> 
            return $ Err UserNotFound


changePwdByID :: Word32 -> ByteString -> Update UserDB (Result Error ())
changePwdByID userID newPwd = do
    ut <- get
    let uDb = users ut
    case IX.getOne $ IX.getEQ (UserID userID) uDb of
        Just head -> do
            put $ ut { 
                users = IX.updateIx 
                        (UserID userID) 
                        (head { pwdHash = Password newPwd }) 
                        uDb
                }
            return $ Ok ()
        _ -> 
            return $ Err UserNotFound


changeGroupsByID :: Word32 -> [Group] -> Update UserDB (Result Error ())
changeGroupsByID userID groups = do
    ut <- get
    let uDb = users ut
    case IX.getOne $ IX.getEQ (UserID userID) uDb of
        Just head -> do
            put $ ut { 
                users = IX.updateIx 
                        (UserID userID) 
                        (head { groups = Groups $ Set.fromList groups }) 
                        uDb
                }
            return $ Ok ()
        _ -> 
            return $ Err UserNotFound

listUsers :: Query UserDB [UserData]
listUsers = do
    ut <- ask
    return $ IX.toList $ users ut


$(makeAcidic ''UserDB [   'lookupPasswordByName
                        , 'lookupGroupsByID
                        , 'lookupIDByUsername
                        , 'addUser
                        , 'removeUser
                        , 'addGroup
                        , 'removeGroup
                        , 'changePwd
                        , 'changeUsernameByID
                        , 'changePwdByID
                        , 'changeGroupsByID
                        , 'listUsers
                      ])

validateUser :: String -> String -> Task Error Bool
validateUser username password = StateTask $ \users -> do
    let uDb = userDb users
    pHash <- query' uDb (LookupPasswordByName $ T.pack username)
    case pHash of
        Just h -> return $ Ok $ validatePassword h (pack password)
        _ -> return $ Err UserNotFound

findUserID :: String -> Task Error Int
findUserID username = StateTask $ \users -> do
    let uDb = userDb users
    muId <- query' uDb (LookupIDByUsername $ T.pack username)
    case muId of
        Ok uId -> return $ Ok (fromIntegral uId)
        Err err -> return $ Err err

insertUser :: String -> String -> [Group] -> Task Error Word32
insertUser username password groups = StateTask $ \users -> do
    let uDb = userDb users
    newPwdHash <- fmap fromJust $ hashPasswordUsingPolicy currentHashingScheme (pack password)
    let newUserData = UserData { 
            username = Username $ T.pack username
        ,   groups = Groups $ Set.fromList groups
        ,   pwdHash = Password newPwdHash
        }
    res <- update' uDb (AddUser (T.pack username) newUserData)
    return res

deleteUser :: String -> Task Error ()
deleteUser username = StateTask $ \users -> do
    let uDb = userDb users
    res <- update' uDb (RemoveUser $ T.pack username)
    return res

assignGroup :: String -> Group -> Task Error ()
assignGroup username group = StateTask $ \users -> do
    let uDb = userDb users
    res <- update' uDb (AddGroup (T.pack username) group)
    return res

revokeGroup :: String -> Group -> Task Error ()
revokeGroup username group = StateTask $ \users -> do
    let uDb = userDb users
    res <- update' uDb (RemoveGroup (T.pack username) group)
    return res

updatePwd :: String -> String -> Task Error ()
updatePwd username password = StateTask $ \users -> do
    let uDb = userDb users
    newPwdHash <- fmap fromJust $ hashPasswordUsingPolicy currentHashingScheme (pack password)
    res <- update' uDb (ChangePwd (T.pack username) newPwdHash)
    return res

updateUsernameByID :: Word32 -> String -> Task Error ()
updateUsernameByID userID newUsername = StateTask $ \users -> do
    res <- update' (userDb users) (ChangeUsernameByID (fromIntegral userID) (T.pack newUsername))
    return res

updatePwdByID :: Word32 -> String -> Task Error ()
updatePwdByID userID password = StateTask $ \users -> do
    let uDb = userDb users
    newPwdHash <- fmap fromJust $ hashPasswordUsingPolicy currentHashingScheme (pack password)
    res <- update' uDb (ChangePwdByID userID newPwdHash)
    return res

updateGroupsByID :: Word32 -> [Group] -> Task Error ()
updateGroupsByID userID groups = StateTask $ \users -> do
    let uDb = userDb users
    res <- update' uDb (ChangeGroupsByID userID groups)
    return res

validateUserGroup :: Group -> Word32 -> Task Error Bool
validateUserGroup group userID = StateTask $ \users -> do
    let uDb = userDb users
    res <- query' uDb (LookupGroupsByID userID)
    return $ Result.map (Set.member group) res

listAllUsers :: Task a [UserData]
listAllUsers = StateTask $ \users -> do
    let uDb = userDb users
    res <- query' uDb ListUsers 
    return $ Ok res

processLogin :: ClientID -> String -> String -> Task Error Bool
processLogin clientID username password =
    validateUser username password 
        |> andThen
            (\success ->
                if success then
                    findUserID username
                        |> andThen 
                            (\userID -> 
                                StateTask $ \users -> do 
                                    atomically $
                                        modifyTVar (sessions users)
                                            (IM.insert clientID (fromIntegral userID))
                                    return $ Ok True
                            )
                else
                    succeed False
            )

lookupUserBySession :: ClientID -> Task Error Word32
lookupUserBySession clientID = StateTask $ \users -> do
    sess <- atomically $ readTVar $ sessions users
    case IM.lookup clientID sess of
        Just uId -> return $ Ok $ fromIntegral uId
        Nothing -> return $ Err UserNotFound
