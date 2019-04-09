{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tapaw.RealWorld.Types
  ( Article(..)
  , Comment(..)
  , LoginUser(..)
  , LoginUserRequest(..)
  , MultipleArticlesResponse(..)
  , MultipleCommentsResponse(..)
  , NewArticle(..)
  , NewArticleRequest(..)
  , NewComment(..)
  , NewCommentRequest(..)
  , NewUser(..)
  , NewUserRequest(..)
  , Profile(..)
  , ProfileResponse(..)
  , SingleArticleResponse(..)
  , SingleCommentResponse(..)
  , TagsResponse(..)
  , UpdateArticleRequest(..)
  , UpdateUserRequest(..)
  , User(..)
  , UserResponse(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


-- * Comment

data Comment = Comment
  { id :: Int
  , createdAt :: Integer
  , updatedAt :: Integer
  , body :: Text
  , author :: Profile
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype NewComment = NewComment
  { body :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype NewCommentRequest = NewCommentRequest
  { comment :: NewComment
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype SingleCommentResponse = SingleCommentResponse
  { comment :: Comment
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype MultipleCommentsResponse = MultipleCommentsResponse
  { comments :: [Comment]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype TagsResponse = TagsResponse
  { tags :: [Text]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- * Article

data Article = Article
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Text]
  , createdAt :: Integer
  , updatedAt :: Integer
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data NewArticle = NewArticle
  { title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Text]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype NewArticleRequest = NewArticleRequest
  { article :: NewArticle
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype UpdateArticleRequest = UpdateArticleRequest
  { article :: NewArticle
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype SingleArticleResponse = SingleArticleResponse
  { article :: Article
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data MultipleArticlesResponse = MultipleArticlesResponse
  { articles :: [Article]
  , articlesCount :: Int
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- * User

data User = User
  { email :: Text
  , token :: Text
  , username :: Text
  , bio :: Text
  , image :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data NewUser = NewUser
  { username :: Text
  , email :: Text
  , password :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Profile = Profile
  { username :: Text
  , bio :: Text
  , image :: Text
  , following :: Bool
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype NewUserRequest = NewUserRequest
  { user :: NewUser
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype UpdateUserRequest = UpdateUserRequest
  { user :: User
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype UserResponse = UserResponse
  { user :: User
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype ProfileResponse = ProfileResponse
  { profile :: Profile
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- * Login

data LoginUser = LoginUser
  { email :: Text
  , password :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype LoginUserRequest = LoginUserRequest
  { user :: LoginUser
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
