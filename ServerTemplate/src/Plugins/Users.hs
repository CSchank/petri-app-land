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

currentHashingScheme = slowerBcryptHashingPolicy
    {
        preferredHashCost = 10
    }
data Group =
      Administrator
    | Supervisor
    | User
    | Custom String
    deriving (Eq,Show,Ord,Data)
$(deriveSafeCopy 0 'base ''Group)

newtype UserID = UserID Word32              deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserID)
newtype Username = Username T.Text          deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Username)
newtype Password = Password ByteString      deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Password)
newtype Groups = Groups (Set.Set Group)     deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Groups)

data UserData = 
    UserData 
        {   userID :: UserID
        ,   username :: Username
        ,   pwdHash :: Password
        ,   groups :: Groups
        }
    deriving(Ord,Eq)
$(deriveSafeCopy 0 'base ''UserData)

instance Indexable UserData where
    empty = ixSet
        [
            ixFun $ \ud -> [userID ud]
        ,   ixFun $ \ud -> [username ud]
        ,   ixFun $ \ud -> [pwdHash ud]
        ,   ixFun $ \ud -> [groups ud]
        ]

-- data to be stored in AcidState instance
data UserDB = 
    UserDB 
        {
            users :: IxSet UserData 
        ,   nextUserID :: Word32
        }
$(deriveSafeCopy 0 'base ''UserDB)



-- plugin instance
data Users =
    Users 
    {
        userDb :: AcidState UserDB
    }

instance (Plugin Users) where
    initPlugin = do
        putStrLn "Loading users database...."
        pwd <- fmap fromJust $ hashPasswordUsingPolicy currentHashingScheme (pack "Pass1")
        let newUser = UserData
                {
                    userID = UserID 0
                ,   username = Username "Chris"
                ,   pwdHash = Password pwd
                ,   groups = Groups Set.empty
                }
        let initialDB = UserDB
                {
                    users = IX.insert newUser IX.empty
                ,   nextUserID = 1
                }
        as <- openLocalState initialDB
        return $ 
            Users
                {
                    userDb = as
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

addUser :: T.Text -> UserData -> Update UserDB Bool
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
            return True
        _ -> 
            return False

removeUser :: T.Text -> Update UserDB Bool
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
            return True
        _ -> 
            return False

addGroup :: T.Text -> Group -> Update UserDB Bool
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
            return True
        _ -> 
            return False

removeGroup :: T.Text -> Group -> Update UserDB Bool
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
            return True
        _ -> 
            return False


changePwd :: T.Text -> ByteString -> Update UserDB Bool
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
            return True
        _ -> 
            return False


updateUsernameByID :: Int -> T.Text -> Update UserDB Bool
updateUsernameByID userID newUsername = do
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
            return True
        _ -> 
            return False


$(makeAcidic ''UserDB [ 'lookupPasswordByName
                        , 'addUser
                        , 'removeUser
                        , 'addGroup
                        , 'removeGroup
                        , 'changePwd
                        , 'updateUsernameByID
                      ])

validateUser :: String -> String -> (Bool -> msg) -> Cmd msg
validateUser username password msgf = StateCmd $ \users -> do
    let uDb = userDb users
    pHash <- query' uDb (LookupPasswordByName $ T.pack username)
    case pHash of
        Just h -> return $ msgf $ validatePassword h (pack password)
        _ -> return $ msgf False

insertUser :: String -> String -> [Group] -> (Bool -> msg) -> Cmd msg
insertUser username password groups msgf = StateCmd $ \users -> do
    let uDb = userDb users
    newPwdHash <- fmap fromJust $ hashPasswordUsingPolicy currentHashingScheme (pack password)
    let newUserData = UserData { 
            username = Username $ T.pack username
        ,   groups = Groups $ Set.fromList groups
        ,   pwdHash = Password newPwdHash
        }
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