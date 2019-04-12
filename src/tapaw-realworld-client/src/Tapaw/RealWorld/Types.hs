{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tapaw.RealWorld.Types
  ( Article(Article)
  , Comment(Comment)
  , LoginUser(LoginUser)
  , LoginUserRequest(LoginUserRequest)
  , MultipleArticlesResponse(MultipleArticlesResponse)
  , MultipleCommentsResponse(MultipleCommentsResponse)
  , NewArticle(NewArticle)
  , NewArticleRequest(NewArticleRequest)
  , NewComment(NewComment)
  , NewCommentRequest(NewCommentRequest)
  , NewUser(NewUser)
  , NewUserRequest(NewUserRequest)
  , Profile(Profile)
  , ProfileResponse(ProfileResponse)
  , SingleArticleResponse(SingleArticleResponse)
  , SingleCommentResponse(SingleCommentResponse)
  , TagsResponse(TagsResponse)
  , UpdateArticleRequest(UpdateArticleRequest)
  , UpdateUser(UpdateUser)
  , UpdateUserRequest(UpdateUserRequest)
  , User(User)
  , UserResponse(UserResponse)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


-- * Comment

data Comment = Comment
  { id :: Int
  , createdAt :: Text
  , updatedAt :: Text
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
  , createdAt :: Text
  , updatedAt :: Text
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
  , bio :: Maybe Text
  , image :: Maybe Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
instance Semigroup User where
  _ <> a = a

data NewUser = NewUser
  { username :: Text
  , email :: Text
  , password :: Text
  , bio :: Text
  , image :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data UpdateUser = UpdateUser
  { username :: Text
  , email :: Text
  , bio :: Text
  , image :: Text
  , password :: Maybe Text
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
  { user :: UpdateUser
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
