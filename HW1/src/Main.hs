module Main where

import Grammar (Expr(..))
import Lexer (alexScanTokens)
import Parser (parseExpr)
import Data.List(intercalate)

getExpr :: String -> Expr
getExpr s =
  case parseExpr (alexScanTokens s) of
    Left err   -> error "ERROR"
    Right expr -> expr

main :: IO ()
main = do
  content    <- getContents
  lines      <- return $! lines content
  unitedLine <- return $! intercalate " " lines
  result     <- return $ getExpr unitedLine
  putStr $ show result
