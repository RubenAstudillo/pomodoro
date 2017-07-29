{-# LANGUAGE DeriveGeneric #-}

module Common (
    PomodoroStatus(Work, Relax, Inactive)
  , Message(..)
  , interpret
  , waitUntil
  , waitUntilTimeout
  ) where

import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.Monad (when)


data PomodoroStatus = Work (Maybe Int) -- timed event or default
                    | Relax | Inactive
  deriving (Show, Generic)

instance Eq PomodoroStatus where
  pom1 == pom2 = case (pom1, pom2) of
                   (Work _, Work _)     -> True
                   (Relax, Relax)       -> True
                   (Inactive, Inactive) -> True
                   _                    -> False

instance Serialize PomodoroStatus


data Message = Message PomodoroStatus (Maybe String)
  deriving (Show, Eq, Generic)

instance Serialize Message

interpret :: PomodoroStatus -> String
interpret (Work _) = "Stay focused!"
interpret Relax    = "Take a break"
interpret Inactive = "The timer is inactive"


waitUntil :: Int -> IO Bool -> IO ()
waitUntil d f = f >>= \success -> when (not success) $ (threadDelay d >> waitUntil d f)
  
waitUntilTimeout :: Int -> Int -> IO Bool -> IO (Maybe ())
waitUntilTimeout t d f
  | t < 0     = return $ Nothing
  | otherwise = f >>= \success -> case success of
      True  -> return $ Just ()
      False -> threadDelay d >> waitUntilTimeout (t-d) d f


