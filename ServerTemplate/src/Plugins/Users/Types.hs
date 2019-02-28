{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveDataTypeable, TypeSynonymInstances #-}

module Plugins.Users.Types where

import qualified Data.Text as T
import Data.Data            (Data, Typeable)
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Set as Set
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..), getOne
                            , ixFun, ixSet )
import Data.Acid

import Static.Result (Result)
import Data.IntMap (IntMap)
import Control.Concurrent.STM.TVar (TVar)

$(deriveSafeCopy 0 'base ''Result)


data Group =
      Administrator
    | Supervisor
    | User
    | Custom T.Text
    deriving (Eq,Show,Ord,Data)
$(deriveSafeCopy 0 'base ''Group)

data Error = 
      UserNotFound
    | InsufficientPermissions
    | MiscError
    deriving(Eq,Show,Ord,Data)
$(deriveSafeCopy 0 'base ''Error)


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
    ,   sessions :: TVar (IntMap Word32)
    }