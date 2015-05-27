{-# LANGUAGE DeriveDataTypeable #-}
module System.Socket.Internal.Socket (
    Socket (..)
  , SocketException (..)
  , GetSockOpt (..)
  , SetSockOpt (..)
  , SO_ACCEPTCONN
  ) where

import Control.Concurrent.MVar
import Control.Exception

import Data.Typeable

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Error
import Foreign.Marshal.Alloc
import System.Posix.Types

import System.Socket.Internal.FFI

#include "sys/socket.h"

-- | A generic socket type. Also see `socket` for details.
--
--   The socket is just an `Control.Concurrent.MVar.MVar`-wrapped file descriptor.
--   It is exposed in order to make this library easily extensible, but it is
--   usually not necessary nor advised to work directly on the file descriptor.
--   If you do, the following rules must be obeyed:
--
--   - Make sure not to deadlock. Use `Control.Concurrent.MVar.withMVar` or similar.
--   - The lock __must not__ be held during a blocking call. This would make it impossible
--     to send and receive simultaneously or to close the socket.
--   - The lock __must__ be held when calling operations that use the file descriptor.
--     Otherwise the socket might get closed or even reused by another
--     thread/capability which might result in reading from or writing
--     totally different connection. This is a security nightmare!
--   - The socket is non-blocking and all the code relies on that assumption.
--     You need to use GHC's eventing mechanism primitives to block until
--     something happens. The former rules forbid to use `GHC.Conc.threadWaitRead` as it
--     does not seperate between registering the file descriptor (for which
--     the lock __must__ be held) and the actual waiting (for which you must
--     __not__ hold the lock).
--     Also see [this](https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html)
--     thread and read the library code to see how the problem is currently circumvented.
newtype Socket d t p
      = Socket (MVar Fd)


newtype SocketException = SocketException Errno
  deriving Typeable

instance Exception SocketException

instance Show SocketException where
  show (SocketException e) = "SocketException " ++ strerr e
    where 
      strerr errno
        | errno == eOK             = "EOK"
        | errno == e2BIG           = "E2BIG"
        | errno == eACCES          = "EACCES"
        | errno == eADDRINUSE      = "EADDRINUSE"
        | errno == eADDRNOTAVAIL   = "EADDRNOTAVAIL"
        | errno == eADV            = "EADV"
        | errno == eAFNOSUPPORT    = "EAFNOSUPPORT"
        | errno == eAGAIN          = "EAGAIN"
        | errno == eALREADY        = "EALREADY"
        | errno == eBADF           = "EBADF"
        | errno == eBADMSG         = "EBADMSG"
        | errno == eBADRPC         = "EBADRPC"
        | errno == eBUSY           = "EBUSY"
        | errno == eCHILD          = "ECHILD"
        | errno == eCOMM           = "ECOMM"
        | errno == eCONNABORTED    = "ECONNABORTED"
        | errno == eCONNREFUSED    = "ECONNREFUSED"
        | errno == eCONNRESET      = "ECONNRESET"
        | errno == eDEADLK         = "EDEADLK"
        | errno == eDESTADDRREQ    = "EDESTADDRREQ"
        | errno == eDIRTY          = "EDIRTY"
        | errno == eDOM            = "EDOM"
        | errno == eDQUOT          = "EDQUOT"
        | errno == eEXIST          = "EEXIST"
        | errno == eFAULT          = "EFAULT"
        | errno == eFBIG           = "EFBIG"
        | errno == eFTYPE          = "EFTYPE"
        | errno == eHOSTDOWN       = "EHOSTDOWN"
        | errno == eHOSTUNREACH    = "EHOSTUNREACH"
        | errno == eIDRM           = "EIDRM"
        | errno == eILSEQ          = "EILSEQ"
        | errno == eINPROGRESS     = "EINPROGRESS"
        | errno == eINTR           = "EINTR"
        | errno == eINVAL          = "EINVAL"
        | errno == eIO             = "EIO"
        | errno == eISCONN         = "EISCONN"
        | errno == eISDIR          = "EISDIR"
        | errno == eLOOP           = "ELOOP"
        | errno == eMFILE          = "EMFILE"
        | errno == eMLINK          = "EMLINK"
        | errno == eMSGSIZE        = "EMSGSIZE"
        | errno == eMULTIHOP       = "EMULTIHOP"
        | errno == eNAMETOOLONG    = "ENAMETOOLONG"
        | errno == eNETDOWN        = "ENETDOWN"
        | errno == eNETRESET       = "ENETRESET"
        | errno == eNETUNREACH     = "ENETUNREACH"
        | errno == eNFILE          = "ENFILE"
        | errno == eNOBUFS         = "ENOBUFS"
        | errno == eNODATA         = "ENODATA"
        | errno == eNODEV          = "ENODEV"
        | errno == eNOENT          = "ENOENT"
        | errno == eNOEXEC         = "ENOEXEC"
        | errno == eNOLCK          = "ENOLCK"
        | errno == eNOLINK         = "ENOLINK"
        | errno == eNOMEM          = "ENOMEM"
        | errno == eNOMSG          = "ENOMSG"
        | errno == eNONET          = "ENONET"
        | errno == eNOPROTOOPT     = "ENOPROTOOPT"
        | errno == eNOSPC          = "ENOSPC"
        | errno == eNOSR           = "ENOSR"
        | errno == eNOSTR          = "ENOSTR"
        | errno == eNOSYS          = "ENOSYS"
        | errno == eNOTBLK         = "ENOTBLK"
        | errno == eNOTCONN        = "ENOTCONN"
        | errno == eNOTDIR         = "ENOTDIR"
        | errno == eNOTEMPTY       = "ENOTEMPTY"
        | errno == eNOTSOCK        = "ENOTSOCK"
        | errno == eNOTTY          = "ENOTTY"
        | errno == eNXIO           = "ENXIO"
        | errno == eOPNOTSUPP      = "EOPNOTSUPP"
        | errno == ePERM           = "EPERM"
        | errno == ePFNOSUPPORT    = "EPFNOSUPPORT"
        | errno == ePIPE           = "EPIPE"
        | errno == ePROCLIM        = "EPROCLIM"
        | errno == ePROCUNAVAIL    = "EPROCUNAVAIL"
        | errno == ePROGMISMATCH   = "EPROGMISMATCH"
        | errno == ePROGUNAVAIL    = "EPROGUNAVAIL"
        | errno == ePROTO          = "EPROTO"
        | errno == ePROTONOSUPPORT = "EPROTONOSUPPORT"
        | errno == ePROTOTYPE      = "EPROTOTYPE"
        | errno == eRANGE          = "ERANGE"
        | errno == eREMCHG         = "EREMCHG"
        | errno == eREMOTE         = "EREMOTE"
        | errno == eROFS           = "EROFS"
        | errno == eRPCMISMATCH    = "ERPCMISMATCH"
        | errno == eRREMOTE        = "ERREMOTE"
        | errno == eSHUTDOWN       = "ESHUTDOWN"
        | errno == eSOCKTNOSUPPORT = "ESOCKTNOSUPPORT"
        | errno == eSPIPE          = "ESPIPE"
        | errno == eSRCH           = "ESRCH"
        | errno == eSRMNT          = "ESRMNT"
        | errno == eSTALE          = "ESTALE"
        | errno == eTIME           = "ETIME"
        | errno == eTIMEDOUT       = "ETIMEDOUT"
        | errno == eTOOMANYREFS    = "ETOOMANYREFS"
        | errno == eTXTBSY         = "ETXTBSY"
        | errno == eUSERS          = "EUSERS"
        | errno == eWOULDBLOCK     = "EWOULDBLOCK"
        | errno == eXDEV           = "EXDEV"
        | otherwise                = let Errno i = errno
                                     in  show i

class GetSockOpt o where
  getSockOpt :: Socket f t p -> IO o

class SetSockOpt o where
  setSockOpt :: Socket f t p -> o -> IO ()

data SO_ACCEPTCONN
   = SO_ACCEPTCONN Bool

instance GetSockOpt SO_ACCEPTCONN where
  getSockOpt (Socket mfd) = do
    withMVar mfd $ \fd->
      alloca $ \vPtr-> do
        alloca $ \lPtr-> do
          i <- c_getsockopt fd (#const SOL_SOCKET) (#const SO_ACCEPTCONN) (vPtr :: Ptr Int) (lPtr :: Ptr Int)
          if i < 0 then do
            throwIO . SocketException =<< getErrno
          else do
            v <- peek vPtr
            return $ SO_ACCEPTCONN (v == 1)