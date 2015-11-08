{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Site where


import           Configuration.Dotenv
import           Control.Arrow
import           Control.Logging
import qualified Data.ByteString            as BS
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Pool
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import qualified Database.PostgreSQL.Simple as PG
import           Network.HTTP.Types.Status
import           Network.Wai
import           System.Environment         (lookupEnv)
import           Web.Fn
import           Web.Fn.Extra.Heist
import Control.Lens
import Database.PostgreSQL.Simple.FromRow

data Ctxt = Ctxt { _req :: Request
                 , _db  :: Pool PG.Connection }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

initializer :: IO Ctxt
initializer = do
  {--let lookupEnv' key def = fmap (fromMaybe def) (lookupUnv key)
  dbhost <- lookupEnv' "DB_HOST" "localhost"
  dbport <- lookupEnv' "DB_PORT" "5432"
  dbuser <- lookupEnv' "DB_USER" "root"
  dbpass <- lookupEnv' "DB_PASS" "123"
  dbname <- lookupEnv' "DB_NAME" "yeslets" --}
  pgpool <- createPool (PG.connect (PG.ConnectInfo "localhost"
                                                   5432
                                                   "libby"
                                                   "123"
                                                   "yeslets"))
                          PG.close 1 60 20
  return (Ctxt defaultRequest pgpool)

app :: IO Application
app = do
  ctxt <- initializer
  return $ toWAI ctxt site

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ path "welcome" ==> welcomeHandler
             , path "signals" ==> signalsHandler
             , path "persons" ==> personsHandler
             , path "login" // segment // segment ==> loginHandler ]
    `fallthrough` notFoundText "Page not found."

data Person = Person { pId :: Int
                     , pName :: Text
                     , pEmail :: Text
                     , pPassword :: Text }
              deriving (Eq, Show)
instance PG.FromRow Person where
  fromRow = Person <$> field <*> field <*> field <*> field

-- 'welcome' is links to signals, persons, login
welcomeHandler = undefined

signalsHandler = undefined

personsQuery :: Pool PG.Connection -> IO [Person]
personsQuery pgpool =
  withResource pgpool (\conn -> 
    PG.query_ conn "SELECT id, name, email, password FROM persons" :: IO [ Person ])

data Login = Login { email    :: Text,
                     password :: Text }

loginPerson :: Login -> Pool PG.Connection -> IO (Maybe Person)
loginPerson login pgpool = 
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, name, email, password FROM persons WHERE email = ? AND password = crypt(?, password)"
         ps <- PG.query conn q (email login, password login) :: IO [Person]
         return $ listToMaybe ps)

findPersonById :: Int -> Pool PG.Connection -> IO (Maybe Person)
findPersonById id' pgpool = 
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, name, email FROM persons WHERE id = ?"
         ps <- PG.query conn q (PG.Only id') :: IO [Person]
         return $ listToMaybe ps)

findPersonByEmail :: Text -> Pool PG.Connection -> IO (Maybe Person)
findPersonByEmail email' pgpool = 
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, name, email FROM persons WHERE email = ?"
         ps <- PG.query conn q (PG.Only email') :: IO [Person]
         return $ listToMaybe ps)

findPersonsByName :: Text -> Pool PG.Connection -> IO [Person]
findPersonsByName name' pgpool = 
  withResource pgpool
    (\conn -> 
         let q = "SELECT id, name, email FROM persons WHERE name = ?" in
         PG.query conn q (PG.Only name') :: IO [Person] )

showT :: Show a => a -> Text
showT = pack . show
  
personsHandler :: Ctxt -> IO (Maybe Response)
personsHandler ctxt =
  route ctxt [ path "id" // segment ==> personsByIdHandler
             , path "name" // segment ==> personsByNameHandler
             , path "email" // segment ==> personsByEmailHandler 
             , anything ==> (\ctxt -> do
                   persons <- personsQuery (_db ctxt)
                   okText (showT persons))]

personsByIdHandler :: Ctxt -> Int -> IO (Maybe Response)
personsByIdHandler ctxt id' = do
  persons <- findPersonById id' (_db ctxt)
  okText (showT persons)

personsByNameHandler :: Ctxt -> Text -> IO (Maybe Response)
personsByNameHandler ctxt name' = do
  persons <- findPersonsByName name' (_db ctxt)
  okText (showT persons)

personsByEmailHandler :: Ctxt -> Text -> IO (Maybe Response)
personsByEmailHandler ctxt email'= do
  persons <- findPersonByEmail email' (_db ctxt)
  okText (showT persons)
             
loginHandler :: Ctxt -> Text -> Text -> IO (Maybe Response)
loginHandler ctxt email pass = do
  maybePerson <- loginPerson (Login email pass) (_db ctxt)
  case maybePerson of
    Just p -> okText (showT p)
    Nothing -> okText "YOU DIDN'T SAY THE MAGIC WORD"