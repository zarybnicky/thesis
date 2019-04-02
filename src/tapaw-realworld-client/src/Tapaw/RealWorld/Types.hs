{-# LANGUAGE DeriveGeneric #-}

module Tapaw.RealWorld.Types
  ( Article(..)
  , Comment(..)
  , GenericErrorModel(..)
  , GenericErrorModel_errors(..)
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
  , UpdateArticle(..)
  , UpdateArticleRequest(..)
  , UpdateUser(..)
  , UpdateUserRequest(..)
  , User(..)
  , UserResponse(..)
  ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Article = Article
  { articleSlug :: Text
  , articleTitle :: Text
  , articleDescription :: Text
  , articleBody :: Text
  , articleTagList :: [Text]
  , articleCreatedAt :: Integer
  , articleUpdatedAt :: Integer
  , articleFavorited :: Bool
  , articleFavoritesCount :: Int
  , articleAuthor :: Profile
  } deriving (Show, Eq, Generic)

instance FromJSON Article where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "article")
instance ToJSON Article where
  toJSON = genericToJSON (removeFieldLabelPrefix False "article")


data Comment = Comment
  { commentId :: Int
  , commentCreatedAt :: Integer
  , commentUpdatedAt :: Integer
  , commentBody :: Text
  , commentAuthor :: Profile
  } deriving (Show, Eq, Generic)

instance FromJSON Comment where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "comment")
instance ToJSON Comment where
  toJSON = genericToJSON (removeFieldLabelPrefix False "comment")


data GenericErrorModel = GenericErrorModel
  { genericErrorModelErrors :: GenericErrorModel_errors
  } deriving (Show, Eq, Generic)

instance FromJSON GenericErrorModel where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "genericErrorModel")
instance ToJSON GenericErrorModel where
  toJSON = genericToJSON (removeFieldLabelPrefix False "genericErrorModel")


data GenericErrorModel_errors = GenericErrorModel_errors
  { genericErrorModelErrorsBody :: [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON GenericErrorModel_errors where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "genericErrorModelErrors")
instance ToJSON GenericErrorModel_errors where
  toJSON = genericToJSON (removeFieldLabelPrefix False "genericErrorModelErrors")


data LoginUser = LoginUser
  { loginUserEmail :: Text
  , loginUserPassword :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON LoginUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loginUser")
instance ToJSON LoginUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loginUser")


data LoginUserRequest = LoginUserRequest
  { loginUserRequestUser :: LoginUser
  } deriving (Show, Eq, Generic)

instance FromJSON LoginUserRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loginUserRequest")
instance ToJSON LoginUserRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loginUserRequest")


data MultipleArticlesResponse = MultipleArticlesResponse
  { multipleArticlesResponseArticles :: [Article]
  , multipleArticlesResponseArticlesCount :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON MultipleArticlesResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "multipleArticlesResponse")
instance ToJSON MultipleArticlesResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "multipleArticlesResponse")


data MultipleCommentsResponse = MultipleCommentsResponse
  { multipleCommentsResponseComments :: [Comment]
  } deriving (Show, Eq, Generic)

instance FromJSON MultipleCommentsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "multipleCommentsResponse")
instance ToJSON MultipleCommentsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "multipleCommentsResponse")


data NewArticle = NewArticle
  { newArticleTitle :: Text
  , newArticleDescription :: Text
  , newArticleBody :: Text
  , newArticleTagList :: [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON NewArticle where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newArticle")
instance ToJSON NewArticle where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newArticle")


data NewArticleRequest = NewArticleRequest
  { newArticleRequestArticle :: NewArticle
  } deriving (Show, Eq, Generic)

instance FromJSON NewArticleRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newArticleRequest")
instance ToJSON NewArticleRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newArticleRequest")


data NewComment = NewComment
  { newCommentBody :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON NewComment where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newComment")
instance ToJSON NewComment where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newComment")


data NewCommentRequest = NewCommentRequest
  { newCommentRequestComment :: NewComment
  } deriving (Show, Eq, Generic)

instance FromJSON NewCommentRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newCommentRequest")
instance ToJSON NewCommentRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newCommentRequest")


data NewUser = NewUser
  { newUserUsername :: Text
  , newUserEmail :: Text
  , newUserPassword :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON NewUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newUser")
instance ToJSON NewUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newUser")


data NewUserRequest = NewUserRequest
  { newUserRequestUser :: NewUser
  } deriving (Show, Eq, Generic)

instance FromJSON NewUserRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newUserRequest")
instance ToJSON NewUserRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newUserRequest")


data Profile = Profile
  { profileUsername :: Text
  , profileBio :: Text
  , profileImage :: Text
  , profileFollowing :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON Profile where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "profile")
instance ToJSON Profile where
  toJSON = genericToJSON (removeFieldLabelPrefix False "profile")


data ProfileResponse = ProfileResponse
  { profileResponseProfile :: Profile
  } deriving (Show, Eq, Generic)

instance FromJSON ProfileResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "profileResponse")
instance ToJSON ProfileResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "profileResponse")


data SingleArticleResponse = SingleArticleResponse
  { singleArticleResponseArticle :: Article
  } deriving (Show, Eq, Generic)

instance FromJSON SingleArticleResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "singleArticleResponse")
instance ToJSON SingleArticleResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "singleArticleResponse")


data SingleCommentResponse = SingleCommentResponse
  { singleCommentResponseComment :: Comment
  } deriving (Show, Eq, Generic)

instance FromJSON SingleCommentResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "singleCommentResponse")
instance ToJSON SingleCommentResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "singleCommentResponse")


data TagsResponse = TagsResponse
  { tagsResponseTags :: [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON TagsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "tagsResponse")
instance ToJSON TagsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "tagsResponse")


data UpdateArticle = UpdateArticle
  { updateArticleTitle :: Text
  , updateArticleDescription :: Text
  , updateArticleBody :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateArticle where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateArticle")
instance ToJSON UpdateArticle where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateArticle")


data UpdateArticleRequest = UpdateArticleRequest
  { updateArticleRequestArticle :: UpdateArticle
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateArticleRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateArticleRequest")
instance ToJSON UpdateArticleRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateArticleRequest")


data UpdateUser = UpdateUser
  { updateUserEmail :: Text
  , updateUserToken :: Text
  , updateUserUsername :: Text
  , updateUserBio :: Text
  , updateUserImage :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateUser")
instance ToJSON UpdateUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateUser")


data UpdateUserRequest = UpdateUserRequest
  { updateUserRequestUser :: UpdateUser
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateUserRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateUserRequest")
instance ToJSON UpdateUserRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateUserRequest")


data User = User
  { userEmail :: Text
  , userToken :: Text
  , userUsername :: Text
  , userBio :: Text
  , userImage :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "user")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "user")


data UserResponse = UserResponse
  { userResponseUser :: User
  } deriving (Show, Eq, Generic)

instance FromJSON UserResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userResponse")
instance ToJSON UserResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userResponse")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (flip ($)) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
