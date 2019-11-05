{-# LANGUAGE FlexibleInstances #-}

module Expression where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Monad (liftM, replicateM)
import Control.Monad.Writer
import Control.Monad.Trans
import System.Random


data BinOp = AddOp
           | SubOp
           | MulOp
           | DivOp
           | RollOp
           deriving Show

type Val = Integer

data Expr = Binary BinOp Expr Expr
          | Value Val
          deriving Show


class (Show a) => PrettyPrintable a where
    prettyPrint :: a -> String

instance PrettyPrintable (Int, String) where
    prettyPrint (i, s) = "Total: " ++ (show i) ++ s

instance PrettyPrintable [Int] where
    prettyPrint lt = foldl (\a b -> a ++ " " ++ b) "" $ map show lt


eofString = "__eof__"

languageDef = emptyDef { Token.reservedNames = [eofString], Token.reservedOpNames = ["d", "+", "-"] }

lexer = Token.makeTokenParser languageDef

integer = Token.integer lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
reserved = Token.reserved lexer

operators = [ 
                [Infix (reservedOp "d" >> return (Binary RollOp)) AssocLeft]
                , [Infix (reservedOp "*" >> return (Binary MulOp)) AssocLeft, Infix (reservedOp "/" >> return (Binary DivOp)) AssocLeft]
                , [Infix (reservedOp "+" >> return (Binary AddOp)) AssocLeft, Infix (reservedOp "-" >> return (Binary SubOp)) AssocLeft]
            ]

expression = buildExpressionParser operators term

term = parens expression
     <|> liftM Value integer

rollDice :: Int -> Int -> WriterT String IO Int
rollDice mult lim = do
    nums <- liftIO . replicateM mult $ getStdRandom (randomR (1, lim))
    let sum = foldl (+) 0 nums 
    tell $ "\n" ++ (show mult) ++ "d" ++ (show lim) ++ " rolled " ++ (show sum) ++ " with rolls" ++ (prettyPrint nums)
    return sum

evaluate :: Expr -> WriterT String IO Int
evaluate expr =
    case expr of
        Value v -> return $ fromIntegral v
        Binary op left right -> do
            l <- evaluate left
            r <- evaluate right
            case op of
                AddOp -> return $ l + r
                SubOp -> return $ l - r
                MulOp -> return $ l * r
                DivOp -> return $ if r /= 0 then l `div` r else 0
                RollOp -> rollDice l r

parseStr = do
    e <- expression
    reserved eofString
    return e

processStr :: String -> IO String
processStr str =
    case parse parseStr "" str of
        Left e -> return $ show e
        Right r -> prettyPrint <$> (runWriterT (evaluate r))