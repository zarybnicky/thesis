{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Tapaw.RealWorld.Client.API
  ( getArticles
  , getArticle
  , getArticleComments
  , tagsGet
  , getArticlesFeed
  , createArticle
  , deleteArticle
  , updateArticle
  , createArticleComment
  , deleteArticleComment
  , createArticleFavorite
  , deleteArticleFavorite
  , followUserByUsername
  , unfollowUserByUsername
  , login
  , createUser
  , getCurrentUser
  , updateCurrentUser
  , getProfileByUsername
  ) where

import Data.Text (Text)
import Reflex.Dom.Core (Dynamic, Event)
import Servant.API
import Servant.Reflex
import Tapaw.RealWorld.Client.Types (AppStateM, getApi)
import Tapaw.RealWorld.Types


getArticles ::
     forall t m a. AppStateM t m
  => Dynamic t (QParam Text)
  -> Dynamic t (QParam Text)
  -> Dynamic t (QParam Text)
  -> Dynamic t (QParam Int)
  -> Dynamic t (QParam Int)
  -> Event t a
  -> m (Event t (ReqResult a MultipleArticlesResponse))
getArticles a b c d e f = do
  (fn :<|> _) <- getApi
  fn a b c d e f

getArticle ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Event t a
  -> m (Event t (ReqResult a SingleArticleResponse))
getArticle a b = do
  (_ :<|> f :<|> _) <- getApi
  f a b

getArticleComments ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Event t a
  -> m (Event t (ReqResult a MultipleCommentsResponse))
getArticleComments a b = do
  (_ :<|> _ :<|> f :<|> _) <- getApi
  f a b

tagsGet ::
     forall t m a. AppStateM t m
  => Event t a
  -> m (Event t (ReqResult a TagsResponse))
tagsGet a = do
  (_ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f a

getArticlesFeed ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (QParam Int)
  -> Dynamic t (QParam Int)
  -> Event t a
  -> m (Event t (ReqResult a MultipleArticlesResponse))
getArticlesFeed a b c d = do
  (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c d

createArticle ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text NewArticleRequest)
  -> Event t a
  -> m (Event t (ReqResult a SingleArticleResponse))
createArticle a b c = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c

deleteArticle ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Text)
  -> Event t a
  -> m (Event t (ReqResult a ()))
deleteArticle a b c = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c

updateArticle ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text UpdateArticleRequest)
  -> Event t a
  -> m (Event t (ReqResult a SingleArticleResponse))
updateArticle a b c d = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c d

createArticleComment ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text NewCommentRequest)
  -> Event t a
  -> m (Event t (ReqResult a SingleCommentResponse))
createArticleComment a b c d = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c d

deleteArticleComment ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Int)
  -> Event t a
  -> m (Event t (ReqResult a ()))
deleteArticleComment a b c d = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c d

createArticleFavorite ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Text)
  -> Event t a
  -> m (Event t (ReqResult a SingleArticleResponse))
createArticleFavorite a b c = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c

deleteArticleFavorite ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Text)
  -> Event t a
  -> m (Event t (ReqResult a SingleArticleResponse))
deleteArticleFavorite a b c = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c

followUserByUsername ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Text)
  -> Event t a
  -> m (Event t (ReqResult a ProfileResponse))
followUserByUsername a b c = do
  (_ :<|>_ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c

unfollowUserByUsername ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Text)
  -> Event t a
  -> m (Event t (ReqResult a ProfileResponse))
unfollowUserByUsername a b c = do
  (_ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c

login ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text LoginUserRequest)
  -> Event t a
  -> m (Event t (ReqResult a UserResponse))
login a b = do
  (_ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f a b

createUser ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text NewUserRequest)
  -> Event t a
  -> m (Event t (ReqResult a UserResponse))
createUser a b = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f a b

getCurrentUser ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Event t a
  -> m (Event t (ReqResult a UserResponse))
getCurrentUser a b = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b

updateCurrentUser ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text UpdateUserRequest)
  -> Event t a
  -> m (Event t (ReqResult a UserResponse))
updateCurrentUser a b c = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) <- getApi
  f (fmap ("Token " <>) <$> a) b c

getProfileByUsername ::
     forall t m a. AppStateM t m
  => Dynamic t (Either Text Text)
  -> Event t a
  -> m (Event t (ReqResult a ProfileResponse))
getProfileByUsername a b = do
  (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f) <- getApi
  f a b
