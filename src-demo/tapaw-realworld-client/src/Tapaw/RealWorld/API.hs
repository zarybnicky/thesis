{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tapaw.RealWorld.API
  ( ConduitAPI
  ) where

import Data.Text (Text)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Tapaw.RealWorld.Types

-- | Servant type-level API, generated from the Swagger spec for Conduit.
type ConduitAPI
  =   "articles"
    :> QueryParam "tag" Text
    :> QueryParam "author" Text
    :> QueryParam "favorited" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Verb 'GET 200 '[JSON] MultipleArticlesResponse -- 'getArticles' route
  :<|> "articles"
    :> Capture "slug" Text
    :> Verb 'GET 200 '[JSON] SingleArticleResponse -- 'getArticle' route
  :<|> "articles"
    :> Capture "slug" Text :> "comments"
    :> Verb 'GET 200 '[JSON] MultipleCommentsResponse -- 'getArticleComments' route
  :<|> "tags"
    :> Verb 'GET 200 '[JSON] TagsResponse -- 'tagsGet' route
  :<|> "articles" :> "feed"
    :> Header "Authorization" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Verb 'GET 200 '[JSON] MultipleArticlesResponse -- 'getArticlesFeed' route
  :<|> "articles"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] NewArticleRequest
    :> Verb 'POST 200 '[JSON] SingleArticleResponse -- 'createArticle' route
  :<|> "articles"
    :> Header "Authorization" Text
    :> Capture "slug" Text
    :> Verb 'DELETE 200 '[JSON] () -- 'deleteArticle' route
  :<|> "articles"
    :> Header "Authorization" Text
    :> Capture "slug" Text
    :> ReqBody '[JSON] UpdateArticleRequest
    :> Verb 'PUT 200 '[JSON] SingleArticleResponse -- 'updateArticle' route
  :<|> "articles"
    :> Header "Authorization" Text
    :> Capture "slug" Text :> "comments"
    :> ReqBody '[JSON] NewCommentRequest
    :> Verb 'POST 200 '[JSON] SingleCommentResponse -- 'createArticleComment' route
  :<|> "articles"
    :> Header "Authorization" Text
    :> Capture "slug" Text :> "comments"
    :> Capture "id" Int
    :> Verb 'DELETE 200 '[JSON] () -- 'deleteArticleComment' route
  :<|> "articles"
    :> Header "Authorization" Text
    :> Capture "slug" Text :> "favorite"
    :> Verb 'POST 200 '[JSON] SingleArticleResponse -- 'createArticleFavorite' route
  :<|> "articles"
    :> Header "Authorization" Text
    :> Capture "slug" Text :> "favorite"
    :> Verb 'DELETE 200 '[JSON] SingleArticleResponse -- 'deleteArticleFavorite' route
  :<|> "profiles"
    :> Header "Authorization" Text
    :> Capture "username" Text :> "follow"
    :> Verb 'POST 200 '[JSON] ProfileResponse -- 'followUserByUsername' route
  :<|> "profiles"
    :> Header "Authorization" Text
    :> Capture "username" Text :> "follow"
    :> Verb 'DELETE 200 '[JSON] ProfileResponse -- 'unfollowUserByUsername' route
  :<|> "users" :> "login"
    :> ReqBody '[JSON] LoginUserRequest
    :> Verb 'POST 200 '[JSON] UserResponse -- 'login' route
  :<|> "users"
    :> ReqBody '[JSON] NewUserRequest
    :> Verb 'POST 200 '[JSON] UserResponse -- 'createUser' route
  :<|> "user"
    :> Header "Authorization" Text
    :> Verb 'GET 200 '[JSON] UserResponse -- 'getCurrentUser' route
  :<|> "user"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] UpdateUserRequest
    :> Verb 'PUT 200 '[JSON] UserResponse -- 'updateCurrentUser' route
  :<|> "profiles"
    :> Capture "username" Text
    :> Verb 'GET 200 '[JSON] ProfileResponse -- 'getProfileByUsername' route
