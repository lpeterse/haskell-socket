{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Main where

import Data.Bits
import Data.Monoid
import Control.Monad
import Control.Exception
import System.Socket
import System.Socket.Family.Inet as Inet
import System.Exit

main :: IO ()
main = do 
  t0001
  t0002
  t0003

t0001 :: IO ()
t0001 = do
  ais <- getAddressInfo
          (Just "127.0.0.1")
          (Just "http")
          aiNumericHost 
          `onException` p 0 :: IO [AddressInfo Inet Stream TCP]
  when (length ais /= 1) (e 1)
  let [ai] = ais
  when (canonicalName ai /= Nothing) (e 2)
  let sa = socketAddress ai
  when (port    sa /= 80) (e 3)
  when (address sa /= Inet.loopback) (e 4)
  where
    p i = print ("t0001." ++ show i)
    e i = error ("t0001." ++ show i)

t0002 :: IO ()
t0002 = do
  let x = getAddressInfo
          Nothing
          Nothing
          mempty :: IO [AddressInfo Inet Stream TCP]
  eui <- tryJust (\ex@(AddressInfoException _)-> if ex == eaiNoName then Just () else Nothing)
                 (x `onException` p 0)
  when (eui /= Left ()) (e 1)
  where
    p i = print ("t0002." ++ show i)
    e i = error ("t0002." ++ show i)

-- | This tests the correct funtionality of the flags
--   AI_V4MAPPEND and AI_ALL. Asking for localhost should
--   yield an additional v4-mappend IPV6-Address in the second case,
--   but not in the first one.
t0003 :: IO ()
t0003 = do
  x <- getAddressInfo
          (Just "localhost")
          Nothing
          mempty 
          `onException` p 0:: IO [AddressInfo Inet6 Stream TCP]
  y <- getAddressInfo
          (Just "localhost")
          Nothing
          (aiAll `mappend` aiV4Mapped)
          `onException` p 1 :: IO [AddressInfo Inet6 Stream TCP]
  when (length x == length y) (e 2)
  where
    p i = print ("t0003." ++ show i)
    e i = error ("t0003." ++ show i)