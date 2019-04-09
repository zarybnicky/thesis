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
import Tapaw.RealWorld.API
import Tapaw.RealWorld.Types


getArticles ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (QParam (Maybe Text))
  -> Dynamic t (QParam (Maybe Text))
  -> Dynamic t (QParam (Maybe Text))
  -> Dynamic t (QParam Int)
  -> Dynamic t (QParam Int)
  -> Event t ()
  -> m (Event t (ReqResult () MultipleArticlesResponse))
getArticles (f :<|> _) = f

getArticle ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () SingleArticleResponse))
getArticle (_ :<|> f :<|> _) = f

getArticleComments ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () MultipleCommentsResponse))
getArticleComments (_ :<|> _ :<|> f :<|> _) = f

tagsGet ::
     forall t m.
     Client t m ConduitAPI ()
  -> Event t ()
  -> m (Event t (ReqResult () TagsResponse))
tagsGet (_ :<|> _ :<|> _ :<|> f :<|> _) = f

getArticlesFeed ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (QParam Int)
  -> Dynamic t (QParam Int)
  -> Event t ()
  -> m (Event t (ReqResult () MultipleArticlesResponse))
getArticlesFeed (_ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

createArticle ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text NewArticleRequest)
  -> Event t ()
  -> m (Event t (ReqResult () SingleArticleResponse))
createArticle (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

deleteArticle ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () ()))
deleteArticle (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

updateArticle ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text UpdateArticleRequest)
  -> Event t ()
  -> m (Event t (ReqResult () SingleArticleResponse))
updateArticle (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

createArticleComment ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text NewCommentRequest)
  -> Event t ()
  -> m (Event t (ReqResult () SingleCommentResponse))
createArticleComment (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

deleteArticleComment ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Int)
  -> Event t ()
  -> m (Event t (ReqResult () ()))
deleteArticleComment (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

createArticleFavorite ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () SingleArticleResponse))
createArticleFavorite (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

deleteArticleFavorite ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () SingleArticleResponse))
deleteArticleFavorite (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

followUserByUsername ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () ProfileResponse))
followUserByUsername (_ :<|>_ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

unfollowUserByUsername ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () ProfileResponse))
unfollowUserByUsername (_ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

login ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text LoginUserRequest)
  -> Event t ()
  -> m (Event t (ReqResult () UserResponse))
login (_ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

createUser ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text NewUserRequest)
  -> Event t ()
  -> m (Event t (ReqResult () UserResponse))
createUser (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

getCurrentUser ::
     forall t m.
     Client t m ConduitAPI ()
  -> Event t ()
  -> m (Event t (ReqResult () UserResponse))
getCurrentUser (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

updateCurrentUser ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text UpdateUserRequest)
  -> Event t ()
  -> m (Event t (ReqResult () UserResponse))
updateCurrentUser (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _) = f

getProfileByUsername ::
     forall t m.
     Client t m ConduitAPI ()
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () ProfileResponse))
getProfileByUsername (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|>_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f) = f
