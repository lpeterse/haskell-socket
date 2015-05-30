module System.Socket.Internal.Msg where

import qualified Data.ByteString.Lazy as LBS

import System.Socket.Internal.MsgFlags

data IoVec

data Msg a t p
   = Msg
     { msgIov     :: LBS.ByteString
     , msgName    :: Maybe a
     , msgControl :: [MsgControl a t p]
     , msgFlags   :: MsgFlags
     }
     deriving (Eq, Show)

-- | Ancillary data.
--
--   According to documentation of @sendmsg@ etc. a message may have ancillary
--   control data for different levels of the stack (IP, SCTP..).
--
--   TODO: Create a data constructor when there's a good plan how to handle all
--   this in a generic way. Till then the empty type is a placeholder and should
--   not be a problem.
data MsgControl a t p

instance Eq (MsgControl a t p)
instance Show (MsgControl a t p)

