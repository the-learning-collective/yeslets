{-# LANGUAGE OverloadedStrings #-}

module Data.JSON.Collection where

import Data.Aeson

type URI = Text

data Collection a =
  Collection {
    cVersion :: Float
  , cHref :: URI
  , cLinks :: [ Link ]
  , cItems :: [ Item a ]
  , cQueries :: [ Query a ]
  , cTemplate :: [ Template a ] }

data Render = Image | Link

data Link =
  { lHref :: URI
  , lRel :: Text
  , lPrompt :: Text
  , lName :: Text
  , lRender :: Render }

data Item a =
  Item {
    iHref :: URI
  , iData :: [ Data a]
  , iLinks :: [ URI ] }

data Data a =
  Data {
    dName :: Text
  , dPrompt :: Text
  , dValue :: Text  }

data Template a = Data a
