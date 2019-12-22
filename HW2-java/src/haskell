{-# LANGUAGE DeriveGeneric #-}

module Main where

import Grammar (Expr(..))
import Lexer (alexScanTokens)
import Parser (parseExpr)
import Data.List(intercalate, find, isSubsequenceOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Splitter



getKAndM :: String -> (Int, Int)
getKAndM line = do
  let list = Splitter.splitOn " " line
  (read (head list) :: Int, read (head (tail list)) :: Int)

getExpr :: String -> Expr
getExpr s = case parseExpr (alexScanTokens s) of
  Left  err  -> error "ERROR"
  Right expr -> expr

rename :: Expr -> Map.Map String String -> Int -> (Expr, Map.Map String String, Int)
rename expr names index =
  case expr of
    Var a -> case Map.lookup a names of
               Just name -> (Var name, names, index)
               Nothing -> (Var a, names, index)
    Application p q -> do
      let (rightExpr, updatedMap, index1) = rename q names index
      let (leftExpr, twiceUpdatedMap, index2) = rename p updatedMap index1
      (Application leftExpr rightExpr, twiceUpdatedMap, index2)
    Abstraction s q -> do
      let newVarName = "v" ++ show (index + 1)
      case Map.lookup s names of -- мб проблеа здесь
        Just x -> do
          let oldName = x
          let updatedMap = Map.insert s newVarName names
          let (qExpr, twiceUpdatedMap, index1) = rename q updatedMap (index + 1)
          let threeTimesUpdatedMap = Map.insert s oldName names
          (Abstraction newVarName qExpr, threeTimesUpdatedMap, index1)
        Nothing -> do
          let updatedMap = Map.insert s newVarName names
          let (qExpr, twiceUpdatedMap, index1) = rename q updatedMap (index + 1)
          let threeTimesUpdatedMap = Map.delete s twiceUpdatedMap
          (Abstraction newVarName qExpr, threeTimesUpdatedMap, index1)

findBetaRedux :: Expr -> Maybe Expr
findBetaRedux (Var a) = Nothing
findBetaRedux (Application p q) =
  case p of
    Abstraction s e -> Just (Application p q)
    _ ->
      case findBetaRedux p of
        Just x -> Just x
        Nothing -> findBetaRedux q
findBetaRedux (Abstraction s q) = findBetaRedux q



getReduxExt :: Expr -> Maybe (Expr, Expr)
getReduxExt (Application p q) = Just (p, q)
getReduxExt _ = Nothing

copySave :: Expr -> Map.Map Expr Expr -> (Expr, Map.Map Expr Expr)
copySave leftRedux copyMap = case leftRedux of
  Var a -> case Map.lookup (Var a) copyMap of
             Just x -> (x, copyMap)
             Nothing -> do
               let updatedMap = Map.insert (Var a) (Var a) copyMap
               (Var a, updatedMap)
  Application p q -> case Map.lookup (Application p q) copyMap of
                       Just x -> (x, copyMap)
                       Nothing -> do
                         let (leftResult, updatedMap) = copySave p copyMap
                         let (rightResult, twiceUpdatedMap) = copySave q updatedMap
                         let threeTimesUpdatedMap = Map.insert (Application p q) (Application leftResult rightResult) twiceUpdatedMap
                         (Application leftResult rightResult, threeTimesUpdatedMap)
  Abstraction s q -> case Map.lookup (Abstraction s q) copyMap of
                       Just x -> (x, copyMap)
                       Nothing -> do
                         let (leftResult, updatedMap) = copySave (Var s) copyMap
                         let (rightResult, twiceUpdatedMap) = copySave q updatedMap
                         let threeTimesUpdatedMap = Map.insert (Abstraction s q) (Abstraction s rightResult) twiceUpdatedMap
                         (Abstraction s rightResult, threeTimesUpdatedMap)

substitute :: Expr -> Maybe String -> Expr -> Expr
substitute left varName right =
  case varName of
    Nothing -> error ""
    Just name ->
      case left of
        Var a ->
          if name == a
            then right
            else left
        Application p q -> Application (substitute p varName right) (substitute q varName right)
        Abstraction s q -> Abstraction s (substitute q varName right)

getVarNameFromLambda :: Expr -> Maybe String
getVarNameFromLambda (Abstraction s q) = Just s
getVarNameFromLambda _ = Nothing

betaReduction :: Expr -> Expr -> Expr -> Expr
betaReduction (Var a) redux reduced = Var a
betaReduction (Application p q) redux reduced =
  if Application p q == redux
    then reduced
    else Application (betaReduction p redux reduced) (betaReduction q redux reduced)
betaReduction (Abstraction s q) redux reduced = Abstraction s (betaReduction q redux reduced)

reduction :: Expr -> Int -> Int -> Int -> Int -> [String] -> [String]
reduction expr m k actual index result
  | m == actual = result
reduction expr m k actual index result = do
  let redux = findBetaRedux expr
  case redux of
    Nothing ->
      if mod (actual - 1) k /= 0
        then result ++ [show expr]
        else result
    Just x -> do
      let (reduxLeft, reduxRight) =
            case getReduxExt x of
              Just (a, b) -> (a, b)
              Nothing -> error "128 line"

      let right = reduxRight

      let (left, _) = case reduxLeft of
                        Abstraction s q -> copySave q Map.empty -- мб здесь нужна мапка
                        _ -> error "131"

--      let result1 = result ++ ["Redux: " ++ show x ++ "\n" ++ show reduxLeft ++ "\n" ++ show reduxRight]
--      let result2 = result1 ++ ["Left: " ++ show left]
--      let result3 = result2 ++ ["Right: " ++ show right]

      let (renamedLeft, updatedMap, index1) = rename left Map.empty index

--      let result4 = result3 ++ ["Renamed left: " ++ show renamedLeft]
--
      let reduced = substitute renamedLeft (getVarNameFromLambda reduxLeft) right

--      let result5 = result4 ++ ["Reduced: " ++ show reduced ++ "\n" ++ show (getVarNameFromLambda reduxLeft)]

      let updatedExpr = betaReduction expr x reduced
--      let updatedResult = if mod actual k == 0 then result5 ++ [show updatedExpr ++ "================="] else result5 ++ ["================"]
      let updatedResult = if mod actual k == 0 then result ++ [show updatedExpr] else result
      reduction updatedExpr m k (actual + 1) index1 updatedResult

main :: IO ()
main = do
  content <- getContents
  lines <- return $! lines content
  let (m, k) = getKAndM $ head lines
  lines <- return $ tail lines
  let expr = getExpr $ head lines
  print expr

--  let (renamedExpr, names, index) = rename expr Map.empty (-1)
--  print renamedExpr
  let result = reduction expr m k 0 (-1) []

  putStrLn $ intercalate " \n" result

