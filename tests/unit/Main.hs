{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (catch)

import Control.Monad.Exception
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Data.IORef
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
    count <- runTestTT tests
    case failures count of
      0 -> exitSuccess
      _ -> exitFailure


tests :: Test
tests = TestList [mkTest (conl ++ " " ++ whatl) con what | (conl, con) <- cons, (whatl, what) <- whats]
  where
    whats :: [(String, ErrorT String IO ())]
    whats = [("return",     return ()),
             ("error",      error "error"),
             ("throwError", throwError "throwError")]

    cons :: [(String, ErrorT String IO () -> ErrorT String IO () -> ErrorT String IO ())]
    cons = [("finally",  \what sequel -> what `finally` sequel),
            ("bracket_", \what sequel -> bracket_ (return ()) sequel what)]

mkTest :: String
       -> (ErrorT String IO () -> ErrorT String IO () -> ErrorT String IO ())
       -> ErrorT String IO ()
       -> Test
mkTest label con what =
    label ~: tst
  where
    tst = do
        ref <- newIORef "sequel not called"
        let sequel = liftIO $ writeIORef ref expected
        _ <- runErrorT (con what sequel) `catch` \(e :: SomeException) -> return (Left (show e))
        actual <- readIORef ref
        return $ assertEqual "" expected actual

    expected :: String
    expected = "sequel called"
