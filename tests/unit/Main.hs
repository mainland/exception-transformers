{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) Geoffrey Mainland 2011-2026
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main where

import Control.Concurrent (forkIO, myThreadId, yield)
import qualified Control.Exception as E
import Control.Monad (void)
import Control.Monad.Exception
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.Error
#endif /* !MIN_VERSION_transformers(0,6,0) */
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.List
#endif
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.Lazy as LRWS
import qualified Control.Monad.Trans.RWS.Strict as SRWS
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.Writer.Lazy as LW
import qualified Control.Monad.Trans.Writer.Strict as SW
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.IORef
import GHC.Conc (BlockReason(..), ThreadStatus(..), threadStatus)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, assertFailure, (@?=))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ exceptTests
        , transformerTests
        , exceptionTTests
#if !MIN_VERSION_transformers(0,6,0)
        , errorTests
#endif /* !MIN_VERSION_transformers(0,6,0) */
        ]

#if !MIN_VERSION_transformers(0,6,0)
errorTests :: Test
errorTests = testGroup "ErrorT tests"
    [testCase (conl ++ " " ++ whatl) (mkErrorTest con what) | (conl, con) <- cons, (whatl, what) <- whats]
  where
    whats :: [(String, ErrorT String IO ())]
    whats = [("return",     return ()),
             ("error",      error "error"),
             ("throwError", throwError "throwError")]

    cons :: [(String, ErrorT String IO () -> ErrorT String IO () -> ErrorT String IO ())]
    cons = [("finally",  \what sequel -> what `finally` sequel),
            ("bracket_", \what sequel -> bracket_ (return ()) sequel what)]

    mkErrorTest :: (ErrorT String IO () -> ErrorT String IO () -> ErrorT String IO ())
                -> ErrorT String IO ()
                -> Assertion
    mkErrorTest con what = do
        ref <- newIORef "sequel not called"
        let sequel = liftIO $ writeIORef ref expected
        _ <- runErrorT (con what sequel) `catch` \(e :: SomeException) -> return (Left (show e))
        actual <- readIORef ref
        expected @?= actual
      where
        expected :: String
        expected = "sequel called"
#endif /* !MIN_VERSION_transformers(0,6,0) */

exceptTests :: Test
exceptTests = testGroup "ExceptT tests"
    [testCase (conl ++ " " ++ whatl) (mkExceptTest con what) | (conl, con) <- cons, (whatl, what) <- whats]
  where
    whats :: [(String, ExceptT String IO ())]
    whats = [("return", return ()),
             ("error",  error "error"),
             ("throwE", throwE "throwE")]

    cons :: [(String, ExceptT String IO () -> ExceptT String IO () -> ExceptT String IO ())]
    cons = [("finally",  \what sequel -> what `finally` sequel),
            ("bracket_", \what sequel -> bracket_ (return ()) sequel what)]

    mkExceptTest :: (ExceptT String IO () -> ExceptT String IO () -> ExceptT String IO ())
                -> ExceptT String IO ()
                -> Assertion
    mkExceptTest con what = do
        ref <- newIORef "sequel not called"
        let sequel = liftIO $ writeIORef ref expected
        _ <- runExceptT (con what sequel) `catch` \(e :: SomeException) -> return (Left (show e))
        actual <- readIORef ref
        expected @?= actual
      where
        expected :: String
        expected = "sequel called"

transformerTests :: Test
transformerTests = testGroup "Transformer cleanup"
    [ liftedTests
    , discardedTests
    , statefulTests
    ]

exceptionTTests :: Test
exceptionTTests = testGroup "ExceptionT restoration"
    [ testCase "pending asynchronous exception runs bracket cleanup" $ do
        releases <- newIORef (0 :: Int)
        let release = liftIO (modifyIORef' releases (+ 1))
        result <- E.try (runExceptionT (bracket_ (liftIO queueInterrupt) release (return ())))
        readIORef releases >>= (@?= 1)
        case result of
          Right (Left e)  -> E.fromException e @?= Just E.UserInterrupt
          Right (Right _) -> assertFailure "the restored action succeeded"
          Left e          -> assertFailure ("the exception escaped ExceptionT: " ++ show (e :: E.AsyncException))
    , testCase "lifted underlying exception runs bracket cleanup" $ do
        releases <- newIORef (0 :: Int)
        let release = liftIO (modifyIORef' releases (+ 1))
            action = lift (E.throwIO (E.ErrorCall "stop")) :: ExceptionT IO ()
        result <- runExceptionT (bracket_ (return ()) release action)
        readIORef releases >>= (@?= 1)
        case result of
          Left e  -> E.fromException e @?= Just (E.ErrorCall "stop")
          Right _ -> assertFailure "the restored action succeeded"
    , testCase "successful restoration keeps underlying state and output" $ do
        let acquire = lift (SRWS.put (1 :: Int) >> SRWS.tell ["acquired"])
            action = lift (SRWS.modify (+ 1) >> SRWS.tell ["used"])
            release = lift (SRWS.modify (+ 1) >> SRWS.tell ["released"])
        (result, state, output) <-
            SRWS.runRWST (runExceptionT (bracket_ acquire release action)) () 0
        case result of
          Left e   -> assertFailure ("the restored action failed: " ++ show e)
          Right () -> return ()
        state @?= 3
        output @?= ["acquired", "used", "released"]
    ]
  where
    -- The acquire action is masked. Wait until throwTo has queued the
    -- exception so it is already pending when the use action restores masking.
    queueInterrupt = do
        target <- myThreadId
        sender <- forkIO (E.throwTo target E.UserInterrupt)
        let wait = do
                status <- threadStatus sender
                case status of
                  ThreadBlocked BlockedOnException -> return ()
                  ThreadFinished                   -> assertFailure "the interrupt was delivered during acquisition"
                  ThreadDied                       -> assertFailure "the interrupt sender died"
                  _                                -> yield >> wait
        wait

-- Each runner except ListT's puts a transformer outside a short-circuiting
-- ExceptT.
liftedTests :: Test
liftedTests = testGroup "Lifted cleanup"
    [ check "IdentityT" (void . runExceptT . runIdentityT) (lift (throwE "stop"))
    , check "ReaderT" (void . runExceptT . flip runReaderT ()) (lift (throwE "stop"))
    , check "MaybeT" (void . runExceptT . runMaybeT) (lift (throwE "stop"))
#if !MIN_VERSION_transformers(0,6,0)
    , check "ListT empty" (void . runListT) (ListT (return []))
    , check "ListT multiple results" (void . runListT) (ListT (return [(), ()]))
#endif
    , masked "IdentityT" runIdentityT
    , masked "ReaderT" (flip runReaderT ())
#if !MIN_VERSION_transformers(0,6,0)
    , masked "ListT" (void . runListT)
#endif
    ]

-- IdentityT, ReaderT, and ListT delegate to the underlying finally, which
-- discards the result of cleanup, so a short circuit in cleanup does not change
-- the result. Nor does it replace an exception that the action raised.
discardedTests :: Test
discardedTests = testGroup "Discarded cleanup short circuit"
    [ testCase "IdentityT over ExceptT" $ do
        result <- runExceptT (runIdentityT (return 1 `finally` lift (throwE "cleanup failed")))
        result @?= (Right 1 :: Either String Int)
    , testCase "ReaderT over ExceptT" $ do
        result <- runExceptT (runReaderT (return 1 `finally` lift (throwE "cleanup failed")) ())
        result @?= (Right 1 :: Either String Int)
    , testCase "ReaderT over MaybeT" $ do
        result <- runMaybeT (runReaderT (return 1 `finally` lift (MaybeT (return Nothing))) ())
        result @?= Just (1 :: Int)
    , testCase "IdentityT over ExceptT rethrows the action's exception" $
        rethrows (runExceptT (runIdentityT (raise `finally` lift (throwE "cleanup failed"))))
    , testCase "ReaderT over ExceptT rethrows the action's exception" $
        rethrows (runExceptT (runReaderT (raise `finally` lift (throwE "cleanup failed")) ()))
#if !MIN_VERSION_transformers(0,6,0)
    , testCase "ListT empty cleanup" $ do
        result <- runListT (ListT (return [1, 2]) `finally` ListT (return []))
        result @?= [1, 2 :: Int]
    , testCase "ListT multiple-result cleanup" $ do
        result <- runListT (ListT (return [1, 2]) `finally` ListT (return [(), ()]))
        result @?= [1, 2 :: Int]
    , testCase "ListT rethrows the action's exception" $
        rethrows (runListT (raise `finally` ListT (return [])))
#endif
    ]

-- The state, writer, and RWS transformers use the default finally. Cleanup
-- runs once after an exception and keeps its effects after success.
statefulTests :: Test
statefulTests = testGroup "Stateful cleanup"
    [ check "StateT lazy exception" (ignoring (void . flip LS.runStateT ())) raise
    , check "StateT strict exception" (ignoring (void . flip SS.runStateT ())) raise
    , check "WriterT lazy exception" (ignoring (void . LW.runWriterT)) (raise :: LW.WriterT [()] IO ())
    , check "WriterT strict exception" (ignoring (void . SW.runWriterT)) (raise :: SW.WriterT [()] IO ())
    , check "RWST lazy exception" (ignoring (\m -> void (LRWS.runRWST m () ()))) (raise :: LRWS.RWST () [()] () IO ())
    , check "RWST strict exception" (ignoring (\m -> void (SRWS.runRWST m () ()))) (raise :: SRWS.RWST () [()] () IO ())
    , testCase "StateT lazy cleanup keeps final state" $ do
        ref <- newIORef 0
        result <- LS.runStateT (LS.put 2 `finally` (LS.get >>= liftIO . writeIORef ref >> LS.put 3)) (1 :: Int)
        result @?= ((), 3)
        readIORef ref >>= (@?= 2)
    , testCase "StateT strict cleanup keeps final state" $ do
        ref <- newIORef 0
        result <- SS.runStateT (SS.put 2 `finally` (SS.get >>= liftIO . writeIORef ref >> SS.put 3)) (1 :: Int)
        result @?= ((), 3)
        readIORef ref >>= (@?= 2)
    , testCase "WriterT lazy cleanup keeps output" $ do
        result <- LW.runWriterT (LW.tell [1 :: Int] `finally` LW.tell [2])
        result @?= ((), [1, 2])
    , testCase "WriterT strict cleanup keeps output" $ do
        result <- SW.runWriterT (SW.tell [1 :: Int] `finally` SW.tell [2])
        result @?= ((), [1, 2])
    , testCase "RWST lazy cleanup keeps final state and output" $ do
        result <- LRWS.runRWST ((LRWS.tell [1 :: Int] >> LRWS.put 2) `finally` (LRWS.tell [2] >> LRWS.modify (+ 1))) () (1 :: Int)
        result @?= ((), 3, [1, 2])
    , testCase "RWST strict cleanup keeps final state and output" $ do
        result <- SRWS.runRWST ((SRWS.tell [1 :: Int] >> SRWS.put 2) `finally` (SRWS.tell [2] >> SRWS.modify (+ 1))) () (1 :: Int)
        result @?= ((), 3, [1, 2])
    , testCase "bracket release keeps state" $ do
        result <- SS.execStateT (bracket_ (SS.modify ("acquired" :)) (SS.modify ("released" :)) (SS.modify ("used" :))) []
        result @?= ["released", "used", "acquired"]
    ]
  where
    ignoring :: (a -> IO ()) -> a -> IO ()
    ignoring run m = run m `E.catch` \(_ :: E.ErrorCall) -> return ()

-- Raise an exception in the underlying IO monad.
raise :: MonadIO m => m ()
raise = liftIO (E.throwIO (E.ErrorCall "stop"))

-- Run cleanup with both finally and bracket_, and check that it runs once.
check :: MonadAsyncException m => String -> (m () -> IO ()) -> m () -> Test
check name run stopped = testGroup name
    [testCase label $ do
        ref <- newIORef (0 :: Int)
        let cleanup = liftIO (modifyIORef' ref (+ 1))
        run (combine stopped cleanup)
        readIORef ref >>= (@?= 1)
    | (label, combine) <- [("finally", finally), ("bracket_", \act sequel -> bracket_ (return ()) sequel act)]]

-- Check that cleanup runs with asynchronous exceptions masked, as the
-- underlying finally in IO runs it.
masked :: MonadAsyncException m => String -> (m () -> IO ()) -> Test
masked name run = testCase (name ++ " cleanup is masked") $ do
    ref <- newIORef E.Unmasked
    run (return () `finally` liftIO (E.getMaskingState >>= writeIORef ref))
    readIORef ref >>= (@?= E.MaskedInterruptible)

-- Check that the exception raised by raise escapes a computation, and that
-- nothing replaces it.
rethrows :: Show a => IO a -> Assertion
rethrows run = do
    result <- E.try run
    case result of
      Left (E.ErrorCall message) -> message @?= "stop"
      Right value                -> assertFailure ("returned " ++ show value)
