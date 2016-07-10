module Lib
    ( AppM
    , runAppM
    ) where

import           Data.Pool               (Pool)
import           Database.Persist.Sqlite (ConnectionPool, SqlBackend, SqlPersistM, runSqlPersistMPool)


type AppM = SqlPersistM

runAppM :: ConnectionPool -> AppM a -> IO a
runAppM pool appM = runSqlPersistMPool appM pool

