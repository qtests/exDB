{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- PartialTypeSignatures

module Main where

import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Logger    (NoLoggingT, runStderrLoggingT)

import           ConfigMod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

-- http://www.parsonsmatt.org/2015/05/02/scotty_and_persistent.html



-- dbFunction :: (BaseBackend backend ~ SqlBackend, IsPersistBackend backend, MonadIO m, MonadBaseControl IO m) => 
--                                                                      ReaderT  backend (NoLoggingT (ResourceT IO)) a -> m a

dbFunction :: ( MonadIO m, MonadBaseControl IO m) =>  ReaderT  SqlBackend (NoLoggingT (ResourceT IO)) a -> m a
dbFunction query = runStderrLoggingT $ 
        withPostgresqlPool connStr 10 $ 
        \pool -> liftIO $ runSqlPersistMPool query pool


doMigrations :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
doMigrations = runMigration migrateAll


doDbStuff :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
doDbStuff = do
        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing

        _ <- insert $ BlogPost "My fr1st p0st" johnId
        _ <- insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])

        john <- get johnId
        liftIO $ print (john :: Maybe Person)


main :: IO ()
main = do
    dbFunction $ runMigration migrateAll
    dbFunction doDbStuff



