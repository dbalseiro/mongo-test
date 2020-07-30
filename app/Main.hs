{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}

import System.Environment
import Data.Default
import Data.Maybe
import Data.Text (pack, unpack)
import Database.MongoDB
import qualified Database.MongoDB.Transport.Tls as TLS
import Network.Socket (HostName)
import Control.Monad.Trans (liftIO)
import Control.Monad ((<=<), void)
import Text.Read

data Settings =
  Settings { dbhost :: HostName
           , dbname :: Database
           , dbport :: PortID
           , dbuser :: Maybe Username
           , dbpass :: Maybe Password
           , dbtls :: Bool
           }

instance Default Settings where
  def = Settings "127.0.0.1" "mydb" defaultPort Nothing Nothing False

main :: IO ()
main = do
  Settings{..} <- getSettings def

  putStrLn $ "Connecting to " ++ show (Host dbhost dbport)

  pipe <- if dbtls
             then TLS.connect dbhost dbport
             else connect (Host dbhost dbport)

  let execute = access pipe master dbname
  let readonly = access pipe slaveOk dbname

  case (dbuser, dbpass) of
    (Nothing, _) -> return ()
    (Just _, Nothing) -> error "password missing"
    (Just user, Just pass) -> void $ execute (authMongoCR user pass)

  void $ readonly run
  close pipe

getSettings :: Settings -> IO Settings
getSettings Settings{..} =
  Settings <$> lookupWithDefault dbhost "DBHOST"
           <*> (pack <$> lookupWithDefault (unpack dbname) "DBNAME")
           <*> (maybe dbport PortNumber . (readMaybe =<<) <$> lookupEnv "DBPORT")
           <*> (fmap pack <$> lookupEnv "DBUSER")
           <*> (fmap pack <$> lookupEnv "DBPASS")
           <*> ((== Just "true") <$> lookupEnv "DBTLS")
    where
      lookupWithDefault d = pure . fromMaybe d <=< lookupEnv

run :: Action IO ()
run = do
  allClients >>= printDocs "All Clients"
  count (select [] "clients") >>= liftIO . print

allClients :: Action IO [Document]
allClients = findCommand (select [] "clients") >>= rest

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs

