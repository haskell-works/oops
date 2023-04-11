{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}

module Examples where

import Control.Monad.Except (MonadError, ExceptT, runExceptT)
import Control.Monad.Oops ( CouldBe, Variant )
import Data.Text (Text)
import Text.Read (readMaybe)
import Data.Functor.Identity (Identity)
import Control.Monad.Trans (MonadIO)
import Data.Function ((&))

import qualified Control.Monad.Oops as OO
import qualified System.IO as IO
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (IOException)
import Control.Monad.Catch (MonadCatch)

-- | A simple function that throws an error.
--
-- The type is the one that is inferred by GHC.
readIntV1 :: ()
  => MonadError (Variant e) m
  => OO.CouldBeF e Text
  => String
  -> m Int
readIntV1 s = case readMaybe @Int s of
  Just i -> return i
  Nothing -> OO.throw @Text "Not an integer"

-- | A simple function that throws an error.
--
-- This is the same as before, but we can rewrite constraint on 'e' differently.
readIntV2 :: ()
  => MonadError (Variant e) m
  => e `CouldBe` Text
  => String
  -> m Int
readIntV2 s = case readMaybe @Int s of
  Just i -> return i
  Nothing -> OO.throw @Text "Not an integer"

-- | A simple function that throws an error.
--
-- We can also use 'ExceptT'
readIntV3 :: ()
  => e `CouldBe` Text
  => String
  -> ExceptT (Variant e) Identity Int
readIntV3 s = case readMaybe @Int s of
  Just i -> return i
  Nothing -> OO.throw @Text "Not an integer"

-- | A simple IO function that throws an error.
--
-- We can also use 'ExceptT' of 'IO'.
readIntV4 :: ()
  => e `CouldBe` Text
  => String
  -> ExceptT (Variant e) IO Int
readIntV4 s = case readMaybe @Int s of
  Just i -> return i
  Nothing -> OO.throw @Text "Not an integer"

-- | A simple function that throws an error.
--
-- Or use MonadIO instead of IO directly.
readIntV5 :: ()
  => MonadError (Variant e) m
  => MonadIO m
  => e `CouldBe` Text
  => String
  -> m Int
readIntV5 s = case readMaybe @Int s of
  Just i -> return i
  Nothing -> OO.throw @Text "Not an integer"

-- | A simple function that throws an error.
--
-- Or use 'MonadIO' with 'ExceptT'.
readIntV6 :: ()
  => MonadIO m
  => e `CouldBe` Text
  => String
  -> ExceptT (Variant e) m Int
readIntV6 s = case readMaybe @Int s of
  Just i -> return i
  Nothing -> OO.throw @Text "Not an integer"

-- We can represent each error as a separate type.

data NotAnInteger = NotAnInteger

data NotPositive = NotPositive

-- | A simple function can throw two errors
readPositiveInt1 :: ()
  => MonadIO m
  => e `CouldBe` NotAnInteger
  => e `CouldBe` NotPositive
  => String
  -> ExceptT (Variant e) m Int
readPositiveInt1 s =
  case readMaybe @Int s of
    Just i ->
      if i > 0
        then return i
        else OO.throw NotPositive
    Nothing -> OO.throw NotAnInteger

-- We can call a function that throws an error of type 'Text' and allow the
-- error to propagate by declaring we also throw an error of type 'Text'.
example1 :: ()
  => e `CouldBe` Text
  => String
  -> ExceptT (Variant e) IO Int
example1 s = do
  i <- readInt s
  liftIO $ IO.print i
  return i
  where
    readInt :: ()
      => e `CouldBe` Text
      => String
      -> ExceptT (Variant e) IO Int
    readInt = error "unimplemented"

-- Or alternatively, we can catch the error of type 'Text' and handle it.
-- in which case the error doesn't propagate.  Notice the 'e CouldBe Text'
-- constraint is not needed in this case.
example2 :: ()
  => String
  -> ExceptT (Variant e) IO Int
example2 s = do
  i <- readInt s
    & OO.catch @Text (\_ -> pure 0)
  liftIO $ IO.print i
  return i
  where
    readInt :: ()
      => e `CouldBe` Text
      => String
      -> ExceptT (Variant e) IO Int
    readInt = error "unimplemented"

-- When we don't throw any errors, we can use the 'runOops' function to
-- convert the 'ExceptT' to an 'IO' action.
example3 :: ()
  => String
  -> IO Int
example3 s = OO.runOops $ do
  i <- readInt s
    & OO.catch @Text (\_ -> pure 0)
  liftIO $ IO.print i
  return i
  where
    readInt :: ()
      => e `CouldBe` Text
      => String
      -> ExceptT (Variant e) IO Int
    readInt = error "unimplemented"

data FileNotFound = FileNotFound

data FileNotReadable = FileNotReadable

data Errors1
  = Errors1NotPositive NotPositive
  | Errors1NotAnInteger NotAnInteger

-- We can call a function that throws multiple errors into a function
-- that only throws one by just catching and rethrowing the one
-- error.
example4 :: ()
  => MonadIO m
  => e `CouldBe` Errors1
  => String
  -> ExceptT (Variant e) m Int
example4 s = do
  i <- readPositiveInt1 s
    & OO.catch @NotPositive (OO.throw . Errors1NotPositive)
    & OO.catch @NotAnInteger (OO.throw . Errors1NotAnInteger)
  liftIO $ IO.print i
  return i

--------------------------------------------------------------------------------
-- Embedding 'Oops' into vanilla 'ExceptT' and 'Either' code.

-- We we have a function that only throws one error, we can use 'runOopsInExceptT'
-- to remove the 'Variant' wrapper leaving only the 'ExceptT'.
example5 :: ()
  => MonadIO m
  => String
  -> ExceptT Errors1 m Int
example5 s = OO.runOopsInExceptT $ do
  i <- readPositiveInt1 s
    & OO.catch @NotPositive (OO.throw . Errors1NotPositive)
    & OO.catch @NotAnInteger (OO.throw . Errors1NotAnInteger)
  liftIO $ IO.print i
  return i

-- We we have a function that only throws one error, we can use 'runOopsInEither'
-- to remove the 'Variant' wrapper and the 'Except' leaving only the 'Either'.
example6 :: ()
  => MonadIO m
  => String
  -> m (Either Errors1 Int)
example6 s = OO.runOopsInEither $ do
  i <- readPositiveInt1 s
    & OO.catch @NotPositive (OO.throw . Errors1NotPositive)
    & OO.catch @NotAnInteger (OO.throw . Errors1NotAnInteger)
  liftIO $ IO.print i
  return i

--------------------------------------------------------------------------------
-- Embedding vanilla 'ExceptT' and 'Either' code into 'Oops' code.
-- We can call a function that throws multiple errors into a function
-- that only throws one by just catching and rethrowing the one
-- error.

-- We can call a function that throws vanilla 'ExceptT' errors and use 'onLeft'
-- to catch and rethrow the errors as oops errors.
example7 :: ()
  => MonadIO m
  => e `CouldBe` NotPositive
  => e `CouldBe` NotAnInteger
  => String
  -> ExceptT (Variant e) m Int
example7 s = do
  i <- readInt s
    & OO.onLeft OO.throw
  pos <- requirePositive i
    & OO.onLeft OO.throw
  liftIO $ IO.print pos
  return pos
  where
    readInt :: MonadIO m => String -> m (Either NotAnInteger Int)
    readInt = error "unimplemented"
    requirePositive :: MonadIO m => Int -> m (Either NotPositive Int)
    requirePositive = error "unimplemented"

-- 'onLeftThrow' is shorthand for 'onLeft throw'.
example8 :: ()
  => MonadIO m
  => e `CouldBe` NotPositive
  => e `CouldBe` NotAnInteger
  => String
  -> ExceptT (Variant e) m Int
example8 s = do
  i <- readInt s
    & OO.onLeftThrow
  pos <- requirePositive i
    & OO.onLeftThrow
  liftIO $ IO.print pos
  return pos
  where
    readInt :: MonadIO m => String -> m (Either NotAnInteger Int)
    readInt = error "unimplemented"
    requirePositive :: MonadIO m => Int -> m (Either NotPositive Int)
    requirePositive = error "unimplemented"

-- We can similarly call a function that throws via vanilla 'ExceptT'
example9 :: ()
  => MonadIO m
  => e `CouldBe` NotPositive
  => e `CouldBe` NotAnInteger
  => String
  -> ExceptT (Variant e) m Int
example9 s = do
  i <- liftIO (runExceptT (readInt s))
    & OO.onLeftThrow
  pos <- liftIO (runExceptT (requirePositive i))
    & OO.onLeftThrow
  liftIO $ IO.print pos
  return pos
  where
    readInt :: String -> ExceptT NotAnInteger IO Int
    readInt = error "unimplemented"
    requirePositive :: Int -> ExceptT NotPositive IO Int
    requirePositive = error "unimplemented"

-- We can similarly call a function that throws via vanilla 'ExceptT'
example10 :: ()
  => MonadIO m
  => e `CouldBe` NotPositive
  => e `CouldBe` NotAnInteger
  => String
  -> ExceptT (Variant e) m Int
example10 s = do
  i <- liftIO (runExceptT (readInt s))
    & OO.onLeftThrow
  pos <- liftIO (runExceptT (requirePositive i))
    & OO.onLeftThrow
  liftIO $ IO.print pos
  return pos
  where
    readInt :: String -> ExceptT NotAnInteger IO Int
    readInt = error "unimplemented"
    requirePositive :: Int -> ExceptT NotPositive IO Int
    requirePositive = error "unimplemented"

-- We can also catch runtime exceptions and rethrow them as checked exceptions.
example11 :: ()
  => MonadIO m
  => MonadCatch m
  => e `CouldBe` IOException
  => ExceptT (Variant e) m String
example11 = do
  i <- liftIO (IO.readFile "moo")
    & OO.onException @IOException OO.throw
  liftIO $ IO.print i
  return i

-- The 'onExceptionThrow' is shorthand for 'onException throw'.
example12 :: ()
  => MonadIO m
  => MonadCatch m
  => e `CouldBe` IOException
  => ExceptT (Variant e) m String
example12 = do
  i <- liftIO (IO.readFile "moo")
    & OO.onExceptionThrow @IOException
  liftIO $ IO.print i
  return i
