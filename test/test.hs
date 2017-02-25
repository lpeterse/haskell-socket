{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.Async    (async, cancel, concurrently, poll,
                                              race, wait)
import           Control.Exception           (bracket, catch, throwIO, try)
import           Control.Monad               (unless, void, when)
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString             as BS
import           Data.Int                    (Int64)
import           Data.Maybe                  (isJust)
import           Data.Monoid                 (mempty, mappend)
import           Prelude                     hiding (head)
import           System.Socket
import           System.Socket.Family.Inet
import           System.Socket.Family.Inet6
import           System.Socket.Protocol.TCP
import           System.Socket.Protocol.UDP
import           System.Socket.Type.Datagram
import           System.Socket.Type.Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck       as QC

main :: IO ()
main  = defaultMain $ testGroup "socket"
  [ testGroup "System.Socket"
    [ group00
    , group01
    , group02
    , group03
    , group07
    , group80
    , group99
    ]
  , testGroup "System.Socket.Inet" [
      group200
    , group201
    ]
  ]

port :: InetPort
port  = 39000

port6 :: Inet6Port
port6 = 39000

group00 :: TestTree
group00 = testGroup "accept"
  [ testGroup "Inet/Strem/TCP"
      [ testCase "cancel operation" $
          -- | This is to test interruptability of (blocking) calls like
          --   accept. The implementation may either run the call "safe"
          --   in another thread if it is really blocking or wait on events
          --   in which case the control is at the RTS's IO manager.
          --   In both cases this test should be able to cancel the accept
          --   async and therefore terminate.
          --   If the test hangs, this means the runtime system got sedated
          --   possibly due to a blocking system call in a non-threaded
          --   environment.
          bracket (socket :: IO (Socket Inet Stream TCP)) close $ \s-> do
            setSocketOption s (ReuseAddress True)
            bind s (SocketAddressInet inetLoopback port)
            listen s 5
            a <- async (accept s)
            threadDelay 1000000 -- make sure the async call really got enough time to start
            p <- poll a
            case p of
              Just (Left ex) -> assertFailure "unexpected exception"
              Just (Right _) -> assertFailure "unexpected connect"
              Nothing        ->  void $ cancel a
      ]
  ]

group01 :: TestTree
group01 = testGroup "connect" [ testGroup "Inet/Stream/TCP" t1 ]
  where
    t1 =
      [ testCase "connect to closed port on inetLoopback" $ bracket
          ( socket :: IO (Socket Inet Stream TCP))
          close
          ( \s-> do
              r <- try $ connect s (SocketAddressInet inetLoopback port)
              case r of
                Left e   | e == eConnectionRefused -> return ()
                         | otherwise               -> throwIO e
                Right () -> assertFailure "connection should have failed"
          )

      , testCase "connect to closed port on inetNone" $ bracket
          ( socket :: IO (Socket Inet Stream TCP))
          close
          ( \s-> do
              r <- try $ connect s (SocketAddressInet inetNone port)
              case r of
                Left e   | e == eNetworkUnreachable  -> return ()
                         | e == eAddressNotAvailable -> return ()
                         | otherwise                 -> throwIO e
                Right () -> assertFailure "connection should have failed"
          )

      , testCase "connect to open socket on localhost" $ bracket
          ( do  server <- socket :: IO (Socket Inet Stream TCP)
                client <- socket :: IO (Socket Inet Stream TCP)
                return (server, client)
          )
          (\(server,client)-> do
                close server
                close client
          )
          (\(server,client)-> do
                setSocketOption server (ReuseAddress True)
                bind server (SocketAddressInet inetLoopback port)
                listen server 5
                connect client (SocketAddressInet inetLoopback port)
          )

      , testCase "connect closed socket" $ bracket
          ( do  server <- socket :: IO (Socket Inet Stream TCP)
                client <- socket :: IO (Socket Inet Stream TCP)
                return (server, client)
          )
          (\(server,client)-> do
                close server
                close client
          )
          (\(server,client)-> do
                setSocketOption server (ReuseAddress True)
                bind server (SocketAddressInet inetLoopback port)
                listen server 5
                close client
                connect client (SocketAddressInet inetLoopback port)
          ) `catch` \e-> case e of
              _ | e == eBadFileDescriptor -> return ()
                | otherwise               -> assertFailure "expected eBadFileDescriptor"

      , testCase "connect already connected socket" $ bracket
          ( do  server <- socket :: IO (Socket Inet Stream TCP)
                client <- socket :: IO (Socket Inet Stream TCP)
                return (server, client)
          )
          (\(server,client)-> do
                close server
                close client
          )
          (\(server,client)-> do
                setSocketOption server (ReuseAddress True)
                bind server (SocketAddressInet inetLoopback port)
                listen server 5
                connect client (SocketAddressInet inetLoopback port)
                connect client (SocketAddressInet inetLoopback port)
                assertFailure "should have thrown eIsConnected"
          ) `catch` \e-> case e of
              _ | e == eIsConnected -> return ()
                | otherwise         -> assertFailure "expected eIsConnected"
      ]

group02 :: TestTree
group02  = testGroup "listen"
  [ testGroup "Inet/Datagram/UDP"
      [ testCase "listen on bound socket" $ bracket
          ( socket :: IO (Socket Inet Datagram UDP) ) close $ \sock-> do
              bind sock (SocketAddressInet inetLoopback port)
              setSocketOption sock (ReuseAddress True)
              listen sock 5 `catch` \e-> case e of
                _ | e == eOperationNotSupported -> return ()
                _ -> assertFailure "expected eOperationNotSupported"
      ]
  , testGroup "Inet/Stream/TCP"
      [ testCase "listen on bound socket" $ bracket
          ( socket :: IO (Socket Inet Stream TCP) ) close $ \sock-> do
              bind sock (SocketAddressInet inetLoopback port)
              setSocketOption sock (ReuseAddress True)
              listen sock 5
      ]
  ]

group03 :: TestTree
group03 = testGroup "send/receive"
  [ testGroup "Inet/Stream/TCP"
    [ testCase "send and receive a chunk" $ bracket
        ( do
          server <- socket :: IO (Socket Inet Stream TCP)
          client <- socket :: IO (Socket Inet Stream TCP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          let addr = SocketAddressInet inetLoopback port
          let helloWorld = "Hello world!"
          setSocketOption server (ReuseAddress True)
          bind server addr
          listen server 5
          serverRecv <- async $ do
            (peerSock, peerAddr) <- accept server
            receive peerSock 4096 mempty
          connect client addr
          send client helloWorld mempty
          msg <- wait serverRecv
          when (msg /= helloWorld) (assertFailure "Received message was bogus.")
        )
    , testCase "recv empty bytestring when peer disconnected gracefully" $ bracket
        ( socket :: IO (Socket Inet Stream TCP) ) close
        (\server-> do
          let addr = SocketAddressInet inetLoopback port
          let msg  = "msg"
          setSocketOption server (ReuseAddress True)
          bind server addr
          listen server 5
          (peer,msg') <- bracket ( socket :: IO (Socket Inet Stream TCP) ) close
            (\client-> do
              connect client addr
              (peer, _) <- accept server
              _ <- send client msg mempty
              msg' <- receive peer 4096 mempty
              return (peer,msg')
            )
          -- The client is disconnected after here.
          when (msg' /= msg) $
            assertFailure "Received message does not match."
          msg'' <- receive peer 4096 mempty
          unless (BS.null msg'') $
            assertFailure "Expected subsequent receives to return empty bytestring."
          msg''' <- receive peer 4096 mempty
          unless (BS.null msg''') $
            assertFailure "Expected subsequent receives to return empty bytestring."
        )
    , testCase "trigger ePipe exception" $ bracket
        ( do
          server <- socket :: IO (Socket Inet Stream TCP)
          client <- socket :: IO (Socket Inet Stream TCP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          let addr = SocketAddressInet inetLoopback port
          setSocketOption server (ReuseAddress True)
          bind server addr
          listen server 5
          connect client addr
          (peerSock, _) <- accept server
          _ <- send client "This should be received." mempty
          _ <- receive peerSock 4096 mempty
          close peerSock
          threadDelay 1000000
          e1 <- try $ send client "This might fail." mempty
          case e1 of
            Right _ -> return ()
            Left  e -> unless (e == ePipe) (throwIO e)
          threadDelay 1000000
          e2 <- try $ send client "This should fail." mempty
          case e2 of
            Right _ -> assertFailure "expected ePipe"
            Left e  -> unless (e == ePipe) (throwIO e)
        )
    ]
  , testGroup "Inet/Datagram/UDP"
    [ testCase "send and receive a datagram" $ bracket
        ( do
          server <- socket :: IO (Socket Inet Datagram UDP)
          client <- socket :: IO (Socket Inet Datagram UDP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          let addr = SocketAddressInet inetLoopback port
          let helloWorld = "Hello world!"
          bind server addr
          ((msg,peeraddr),_) <- concurrently (receiveFrom server 4096 mempty) $ do
            -- This is a race condition:
            --   The server must listen before the client sends his msg or the packt goes
            --   to nirvana. Still, a second here should be enough. If not, there's
            --   something wrong worth investigating.
            threadDelay 1000000
            sendTo client helloWorld mempty addr
          when (msg /= helloWorld) $ assertFailure "messages not equal"
        )
    ]
  ]

group07 :: TestTree
group07 = testGroup "sendAll/receiveAll"
  [ testGroup "Inet/Stream/TCP"
    [ testCase "sendAll and receiveAll a 128MB chunk" $ bracket
        ( do
          server <- socket :: IO (Socket Inet Stream TCP)
          client <- socket :: IO (Socket Inet Stream TCP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          let addr    = SocketAddressInet inetLoopback port
          let msgSize = 128*1024*1024 + 1 :: Int64
          let msg     = LBS.replicate msgSize 23
          setSocketOption server (ReuseAddress True)
          bind server addr
          listen server 5
          serverRecv <- async $ do
            (peerSock, peerAddr) <- accept server
            receiveAll peerSock msgSize mempty
          threadDelay 100000
          connect client addr
          sent <- sendAll client (LBS.toStrict msg) mempty
          close client
          msgReceived <- wait serverRecv
          when (fromIntegral sent /= LBS.length msg) (assertFailure "sendAll reported wrong size.")
          when (msgReceived /= msg) (assertFailure "Received message was bogus.")
        )
    , testCase "sendAllLazy and receiveAll a 128MB chunk" $ bracket
        ( do
          server <- socket :: IO (Socket Inet Stream TCP)
          client <- socket :: IO (Socket Inet Stream TCP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          let addr    = SocketAddressInet inetLoopback port
          let msgSize = 128*1024*1024 + 1 :: Int64
          let msg     = LBS.replicate msgSize 23
          setSocketOption server (ReuseAddress True)
          bind server addr
          listen server 5
          serverRecv <- async $ do
            (peerSock, peerAddr) <- accept server
            receiveAll peerSock msgSize mempty
          threadDelay 100000
          connect client addr
          sent <- sendAllLazy client msg mempty
          close client
          msgReceived <- wait serverRecv
          when (sent /= LBS.length msg) (assertFailure "sendAllLazy reported wrong size.")
          when (msgReceived /= msg) (assertFailure "Received message was bogus.")
        )
    , testCase "sendAllBuilder and receiveAll a 128MB chunk" $ bracket
        ( do
          server <- socket :: IO (Socket Inet Stream TCP)
          client <- socket :: IO (Socket Inet Stream TCP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          let addr    = SocketAddressInet inetLoopback port
          let msgSize = 128*1024*1024 + 1 :: Int64
          let msg     = LBS.replicate msgSize 23
          setSocketOption server (ReuseAddress True)
          bind server addr
          listen server 5
          serverRecv <- async $ do
            (peerSock, peerAddr) <- accept server
            receiveAll peerSock msgSize mempty
          threadDelay 100000
          connect client addr
          sent <- sendAllBuilder client 512 (foldr (\bs-> (BB.byteString bs `mappend`)) mempty $ LBS.toChunks msg) mempty
          close client
          msgReceived <- wait serverRecv
          when (sent /= LBS.length msg) (assertFailure "sendAllBuilder reported wrong size.")
          when (msgReceived /= msg) (assertFailure "Received message was bogus.")
        )
    ]
  ]

group80 :: TestTree
group80 = testGroup "setSocketOption" [ testGroup "V6Only"
    [ testCase "present" $ bracket
        ( do
          server <- socket :: IO (Socket Inet6 Datagram UDP)
          client <- socket :: IO (Socket Inet Datagram UDP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          setSocketOption server (V6Only True)
          setSocketOption server (ReuseAddress True)
          bind server (SocketAddressInet6 inet6Any port6 0 0)
          threadDelay 1000000 -- wait for the listening socket being set up
          sendTo client "PING" mempty (SocketAddressInet inetLoopback port)
          eith <- race
            ( void $ receiveFrom server 4096 mempty )
            ( threadDelay 1000000 )
          case eith of
            Left ()  -> assertFailure "expected timeout"
            Right () -> return ()  -- timeout is the expected behaviour
        )
    , testCase "absent" $ bracket
        ( do
          server <- socket :: IO (Socket Inet6 Datagram UDP)
          client <- socket :: IO (Socket Inet Datagram UDP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          setSocketOption server (V6Only False)
          setSocketOption server (ReuseAddress True)
          bind server (SocketAddressInet6 inet6Any port6 0 0)
          threadDelay 1000000 -- wait for the listening socket being set up
          sendTo client "PING" mempty (SocketAddressInet inetLoopback port)
          eith <- race
            ( void $ receiveFrom server 4096 mempty )
            ( threadDelay 1000000 )
          case eith of
            Left ()  -> return ()
            Right () -> assertFailure "expected packet"
        )
    ]
  ]

group99 :: TestTree
group99  = testGroup "getAddrInfo" [

    testCase "getAddrInfo \"127.0.0.1\" \"80\"" $ do
      ais <- getAddressInfo
              (Just "127.0.0.1")
              (Just "80")
              aiNumericHost :: IO [AddressInfo Inet Stream TCP]
      when (length ais /= 1) $ assertFailure "expected 1 result"
      let [ai] = ais
      when (isJust $ canonicalName ai) $ assertFailure "expected no canonical name"
      let sa = socketAddress ai
      when (inetAddress sa /= inetLoopback) $ assertFailure "expected loopback address"
      when (inetPort    sa /= 80) $ assertFailure "expected port 80"

  , testCase "getAddrInfo \"\" \"\"" $
      void (getAddressInfo Nothing Nothing mempty :: IO [AddressInfo Inet Stream TCP]) `catch` \e-> case e of
            _ | e == eaiNoName -> return ()
            _ -> assertFailure "expected eaiNoName"

  ]

group200 :: TestTree
group200 = testGroup "System.Socket.Family.Inet" [

    testCase "inetAddressFromTuple (127,0,0,1) == inetLoopback" $
      assertEqual "" ( inetAddressFromTuple (127,0,0,1) ) inetLoopback

  , QC.testProperty  "inetAddressToTuple (inetAddressFromTuple x) == x" $ \x->
      inetAddressToTuple (inetAddressFromTuple x) === x
  ]

group201 :: TestTree
group201 = testGroup "System.Socket.Family.Inet6" [

    testCase "inet6AddressFromTuple (0,0,0,0,0,0,0,1) == inet6Loopback" $
      assertEqual "" ( inet6AddressFromTuple (0,0,0,0,0,0,0,1) ) inet6Loopback

  , QC.testProperty  "inet6AddressToTuple (inet6AddressFromTuple x) == x" $ \x->
      inet6AddressToTuple (inet6AddressFromTuple x) === x
  ]
