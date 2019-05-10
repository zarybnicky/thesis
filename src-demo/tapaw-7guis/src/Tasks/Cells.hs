{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tasks.Cells
  ( cells
  ) where

import Control.Lens hiding (levels)
import Control.Monad (forM)
import Control.Monad.Trans.State.Lazy (execState)
import Control.Monad.State (MonadState)
import Data.Array ((//))
import Data.Bool (bool)
import Data.Char (ord, toLower)
import Data.Graph (Graph, Vertex, buildG, dfs, scc, transposeG)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Tree (flatten, levels)
import Data.Void (Void)
import Data.Generics.Product (field)
import GHC.Generics (Generic)
import Reflex.Dom.Core
import Text.Megaparsec
import Text.Megaparsec.Char (letterChar, numberChar, space, space1)
import Text.Megaparsec.Expr (Operator(..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L


data Coord = Coord Int Int
  deriving (Eq, Ord, Show)

data Expr
  = ERef Coord
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | ENegate Expr
  | ENumber Float
  deriving (Show)

data CellResult
  = CellNumber Float
  | CellError CellError
  deriving (Show)

data CellError
  = CellEmpty
  | CellParseError String
  | CellRefNotFound Coord
  | CellDependentError Coord
  | CellInvalidRefs Text
  | CellDivByZero
  deriving (Show)

data Spreadsheet = Spreadsheet
  { sSize :: Coord
  , sContent :: Map Coord (Maybe Expr, CellResult)
  , sRefs :: Graph
  } deriving (Show, Generic)

coordToVertex :: Coord -> Coord -> Vertex
coordToVertex (Coord w _) (Coord a b) = b * w + a

coordFromVertex :: Coord -> Vertex -> Coord
coordFromVertex (Coord w _) x = Coord (x `mod` w) (x `div` w)

updateCell :: Coord -> Text -> Spreadsheet -> Spreadsheet
updateCell c inp s = flip execState s $ do
  size <- use (field @"sSize")

  if inp == ""
    then do
      field @"sContent" . at c . _Just . _2 .= CellError CellEmpty
      field @"sRefs" %= (// [(coordToVertex size c, [])])
    else case parse (space >> expr) "" inp of
      Left e -> do
        field @"sContent" . at c . _Just . _2 .= CellError (CellParseError $ parseErrorPretty e)
        field @"sRefs" %= (// [(coordToVertex size c, [])])
      Right e -> do
        field @"sContent" . at c . _Just . _1 ?= e
        let deps = filter (inBounds size) (getExprDeps e)
            refs' = sRefs s // [(coordToVertex size c, coordToVertex size <$> deps)]
        if | c `elem` deps ->
              field @"sContent" . at c . _Just . _2 .= CellError (CellInvalidRefs "Self-reference")
           | hasCycle refs' ->
              field @"sContent" . at c . _Just . _2 .= CellError (CellInvalidRefs "Cyclical reference")
           | otherwise -> do
             field @"sRefs" .= refs'
             res <- evalCell e
             field @"sContent" . at c . _Just . _2 .= res

  refs <- use (field @"sRefs")
  forM (maybe [] tail (levelsAtVertex size refs c)) $ \level ->
    forM level $ \c' ->
      preuse (field @"sContent" . at c' . _Just . _1 . _Just) >>= \case
        Nothing -> pure ()
        Just e' -> evalCell e' >>= (field @"sContent" . at c' . _Just . _2 .= )

hasCycle :: Graph -> Bool
hasCycle = not . all ((<= 1) . length . flatten) . scc

levelsAtVertex :: Coord -> Graph -> Coord -> Maybe [[Coord]]
levelsAtVertex size g v =
  case dfs (transposeG g) [coordToVertex size v] of
    [tree] -> Just $ levels $ coordFromVertex size <$> tree
    _ -> Nothing

evalCell :: MonadState Spreadsheet m => Expr -> m CellResult
evalCell (ERef c) = preuse (field @"sContent" . at c . _Just . _2) >>= \case
  Nothing -> pure $ CellError (CellRefNotFound c)
  Just (CellNumber x) -> pure $ CellNumber x
  Just (CellError CellEmpty) -> pure $ CellError (CellRefNotFound c)
  Just (CellError _) -> pure $ CellError (CellDependentError c)
evalCell (ENegate x) = mapCellResult negate <$> evalCell x
evalCell (ENumber x) = pure (CellNumber x)
evalCell (EAdd x y) = combineCellResult ((CellNumber .) . (+)) <$> evalCell x <*> evalCell y
evalCell (ESub x y) = combineCellResult ((CellNumber .) . (-)) <$> evalCell x <*> evalCell y
evalCell (EMul x y) = combineCellResult ((CellNumber .) . (*)) <$> evalCell x <*> evalCell y
evalCell (EDiv x y) = combineCellResult safeDiv <$> evalCell x <*> evalCell y
  where
    safeDiv _ 0 = CellError CellDivByZero
    safeDiv a b = CellNumber $ a / b

mapCellResult :: (Float -> Float) -> CellResult -> CellResult
mapCellResult f (CellNumber x) = CellNumber (f x)
mapCellResult _ x = x

combineCellResult :: (Float -> Float -> CellResult) -> CellResult -> CellResult -> CellResult
combineCellResult f (CellNumber x) (CellNumber y) = f x y
combineCellResult _ x@(CellError _) _ = x
combineCellResult _ _ y@(CellError _) = y

getExprDeps :: Expr -> [Coord]
getExprDeps (ERef c) = [c]
getExprDeps (ENegate x) = getExprDeps x
getExprDeps (ENumber _) = []
getExprDeps (EAdd x y) = getExprDeps x ++ getExprDeps y
getExprDeps (ESub x y) = getExprDeps x ++ getExprDeps y
getExprDeps (EMul x y) = getExprDeps x ++ getExprDeps y
getExprDeps (EDiv x y) = getExprDeps x ++ getExprDeps y

cells :: forall t m. MonadWidget t m => m ()
cells = do
  let initial = Spreadsheet
        (Coord 2 2)
        (M.fromList [ (Coord 0 0, (Nothing, CellError CellEmpty))
                    , (Coord 0 1, (Nothing, CellError CellEmpty))
                    , (Coord 1 0, (Nothing, CellError CellEmpty))
                    , (Coord 1 1, (Nothing, CellError CellEmpty))
                    ])
        (buildG (0, 3) [])
  rec dSpreadsheet <- foldDyn (uncurry updateCell) initial eCell
      eCell <- selectViewListWithKey (pure (Coord 0 0)) (sContent <$> dSpreadsheet) cell
  display dSpreadsheet

cell ::
     MonadWidget t m
  => Coord
  -> Dynamic t (Maybe Expr, CellResult)
  -> Dynamic t Bool
  -> m (Event t Text)
cell _ dResult _ = elAttr "div" ("class" =: "cell") $ do
  (resultBox, ()) <- el' "div" $ display dResult
  rec
    textbox <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes
        .~ ("style" =: "display:none")
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes
        .~ (("style" =:) . Just . bool "display:none" "" <$> eSelected)
    let eSelected = leftmost
          [ True <$ domEvent Dblclick resultBox
          , False <$ keypress Enter textbox
          ]
  pure $ current (value textbox) <@ keypress Enter textbox


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

term :: Parser Expr
term = between (L.symbol sc "(") (L.symbol sc ")") expr
   <|> (ENumber . fromInteger <$> L.lexeme sc L.decimal)
   <|> (ENumber <$> L.lexeme sc L.float)
   <|> (ERef <$> coord) <?> "term"

table :: [[Operator Parser Expr]]
table = [ [ Prefix (ENegate <$ L.symbol sc "-")
          , Prefix (id <$ L.symbol sc "+") ]
        , [ InfixL (EMul <$ L.symbol sc "*")
          , InfixL (EDiv <$ L.symbol sc "/") ]
        , [ InfixL (EAdd <$ L.symbol sc "+")
          , InfixL (EAdd <$ L.symbol sc "-") ]
        ]

coord :: Parser Coord
coord = Coord
  <$> ((ord 'a' `subtract`) . ord . toLower <$> L.lexeme sc letterChar)
  <*> ((ord '0' `subtract`) . ord <$> L.lexeme sc numberChar)

inBounds :: Coord -> Coord -> Bool
inBounds (Coord w h) (Coord i j) = i < w && j < h && i >= 0 && j >= 0
