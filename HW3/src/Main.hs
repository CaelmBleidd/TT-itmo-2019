{-# LANGUAGE DeriveGeneric #-}

module Main where

import Grammar (Expr(..))
import Lexer (alexScanTokens)
import Parser (parseExpr)
import Data.List(intercalate, find, isSubsequenceOf)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable.Class
import GHC.Generics

data Alg_term = Atom Int | Impl Alg_term Alg_term deriving (Eq, Generic)


data Equation = Equation {
  left  :: Alg_term,
  right :: Alg_term } deriving (Eq, Generic)

instance Hashable Alg_term

instance Hashable Equation

instance Show Alg_term where
  show (Atom a) = "t" ++ show a
  show (Impl a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

applySubstitution :: Alg_term -> [Equation] -> Alg_term
applySubstitution (Atom a) solution = case (find (\elem -> left elem == (Atom a)) solution) of
  Nothing -> Atom a
  Just rule -> right rule
applySubstitution (Impl a b) solution = Impl (applySubstitution a solution) (applySubstitution b solution)

instance Show Equation where
 show equation = "Left part: " ++ (show (left equation)) ++ ". Right part: " ++ (show (right equation))

getSystem :: Expr -> Int -> HashMap String Int -> HashMap String Int -> ([Equation], Alg_term, Int, HashMap String Int, HashMap String Int)
getSystem (Var s) index map_first map_second = do
  case HashMap.lookup s map_second of
     Just x -> ([], Atom(x), (index + 1), map_first, map_second)
     Nothing -> case HashMap.lookup s map_first of
                  Just x -> ([], Atom(x), (index + 1), map_first, map_second)
                  Nothing -> ([], Atom(index + 1), (index + 1), HashMap.insert s (index + 1) map_first, map_second)

getSystem (Application p q) index map_first map_second = do
  let (e_p, t_p, index_1, new_first_map, new_second_map) = getSystem p index map_first map_second
  let (e_q, t_q, index_2, twice_updated_first_map, twice_updated_second_map) = getSystem q index_1 new_first_map new_second_map
  let new_e = Equation{left = t_p, right = Impl t_q (Atom (index_2 + 1)) }
  let e = new_e : reverse (e_q ++ e_p)
  (e, Atom(index_2 + 1), (index_2 + 1), twice_updated_first_map, twice_updated_second_map)

getSystem (Abstraction s p) index map_first map_second = do
  let new_second_map = HashMap.insert s (index + 1) map_second
  let (e, t_p, index_1, new_first_map, one_more_second_map) = getSystem p (index + 1) map_first new_second_map
  (e, Impl (Atom (index + 1)) t_p, index_1, new_first_map, HashMap.delete s one_more_second_map)

getExpr :: String -> Expr
getExpr s =
  case parseExpr (alexScanTokens s) of
    Left err   -> error "ERROR"
    Right expr -> expr

hasAtom :: Alg_term -> Alg_term -> Bool
hasAtom (Atom a) atom = (Atom a) == atom
hasAtom (Impl a b) atom = (hasAtom a atom) || (hasAtom b atom)

systemBroker :: Equation -> Bool
systemBroker (Equation (Atom a) (Impl b c)) = hasAtom (Impl b c) (Atom a)
systemBroker _  = False

removeImpl :: Equation -> [Equation]
removeImpl (Equation (Impl a b) (Impl c d)) = [Equation{left = a, right = c}, Equation{left = b, right = d}]
removeImpl other = [other]

removeVarRight :: Equation -> Equation
removeVarRight (Equation (Impl a b) (Atom c)) = Equation{left = Atom (c), right = Impl a b}
removeVarRight other = other

notEqual :: Equation -> Bool
notEqual (Equation (Atom a) (Atom b))
  | a == b = False
notEqual _ = True

isSubstitution :: HashMap Alg_term Bool -> Equation -> Bool
isSubstitution substed (Equation (Atom a) (b)) = not $ HashMap.member (Atom a) substed
isSubstitution _ _ = False

makeSubstitution :: Equation -> Alg_term -> Alg_term
makeSubstitution rule (Atom a)
 | (left rule) == Atom a = right rule
makeSubstitution rule (Impl a b) = Impl (makeSubstitution rule a) (makeSubstitution rule b)
makeSubstitution _ term = term



doSubstitution :: Equation -> Equation -> Equation
doSubstitution rule equal = if (rule == equal)
                               then equal
                               else Equation{left = (makeSubstitution rule (left equal)), right = (makeSubstitution rule (right equal))}

solveSystem :: [Equation] -> HashMap Alg_term Bool -> Maybe [Equation]
solveSystem system substed =
  if any systemBroker system
    then Nothing
    else do
      let previous_state = system
      updated_system <- return $ concatMap removeImpl system
      updated_system <- return $ map removeVarRight updated_system
      updated_system <- return $ filter notEqual updated_system
      case find (isSubstitution substed) updated_system of
        Nothing -> if length previous_state == length updated_system
                   then if isSubsequenceOf previous_state updated_system
                          then Just updated_system
                          else solveSystem updated_system substed
                   else solveSystem updated_system substed
        Just rule -> do
          let updated_substed = HashMap.insert (left rule) True substed
          updated_system <- return $ map (doSubstitution rule) updated_system
          solveSystem updated_system updated_substed



showResult :: [Equation] -> IO()
showResult [] = putStr ""
showResult (x:xs) = do
  putStrLn (show x)
  showResult xs

getContext :: [Equation] -> HashMap String Int -> String
getContext solution map_first = do
  let pairs = HashMap.toList map_first
  let index_to_type_s ind = show (applySubstitution (Atom ind) solution)
  let pair_to_elem (k, v) = intercalate " " [k ++ ":" ++ (index_to_type_s v)]
  intercalate ", " $ map pair_to_elem pairs



showStringResult :: [String] -> IO()
showStringResult [] = putStr "\n"
showStringResult (x:xs) = do
  putStrLn (show x)
  showStringResult xs

createOutput :: Expr -> [Equation] -> Int -> HashMap String Int -> HashMap String Int -> String -> Int -> (String, Alg_term, [String], HashMap String Int, HashMap String Int, Int)
createOutput expr solution height map_first map_second context index = do
  let line = (intercalate "" (replicate height "*   ")) ++ context
  let new_line = if length context == 0 then line ++ "|- " else line ++ " |- "
  case expr of
    Var a -> do
      let id = case HashMap.lookup a map_second of
                 Just x -> x
                 Nothing -> case HashMap.lookup a map_first of
                              Just x -> x
                              Nothing -> error ""
      let s_type = applySubstitution (Atom id) solution
      let result_line = new_line ++ a ++ " : " ++ show s_type ++ " [rule #1]"
      (a, s_type, [result_line], map_first, map_second, index + 1)
    Application p q -> do
      let (p_s, p_type, p_result, updated_map_first, updated_map_second, index_1) = createOutput p solution (height + 1) map_first map_second context index
      let (q_s, q_type, q_result, twice_updated_first_map, twice_updated_second_map, index_2) = createOutput q solution (height + 1) updated_map_first updated_map_second context index_1
      let tail_result = p_result ++ q_result
      let cur_name = "(" ++ p_s ++ " " ++ q_s ++ ")"
      let cur_type = case p_type of
                       Impl a b -> b
                       otherwise -> error ""
      let cur_line = new_line ++ cur_name ++ " : " ++ show cur_type ++ " [rule #2]"
      (cur_name, cur_type, [cur_line] ++ tail_result, twice_updated_first_map, twice_updated_second_map, index_2 + 1)
    Abstraction s p -> do
      let updated_second_map = HashMap.insert s (index + 1) map_second
      let s_type = applySubstitution (Atom (index + 1)) solution
      let tail_context = context ++ (if length context == 0 then "" else ", ") ++ s ++ " : " ++ show s_type
      let (p_s, p_type, tail_proof, updated_map_first, twice_updated_map_second, index_1) = createOutput p solution (height + 1) map_first updated_second_map tail_context (index + 1)
      let three_times_updated_second_table = HashMap.delete s twice_updated_map_second
      let cur_name = "(\\" ++ s ++ ". " ++ p_s ++ ")"
      let cur_line = new_line ++ cur_name ++ " : (" ++ show s_type ++ " -> " ++ show p_type ++ ") [rule #3]"
      (cur_name, Impl s_type p_type, [cur_line] ++ tail_proof, updated_map_first, three_times_updated_second_table, index_1)




getProof :: Expr -> [Equation] -> HashMap String Int -> String
getProof expr solution map_first = do
  let map_second = HashMap.empty
  let context = getContext solution map_first
--  context
  let (cur_s, cur_t, cur_proof, first_map, second_map, index) = createOutput expr solution 0 map_first map_second context 0
  intercalate "\n" cur_proof

main :: IO ()
main = do
  content    <- getContents
  lines      <- return $! lines content
  expr     <- return $ getExpr $ head lines

  let map_first = HashMap.empty
  let map_second = HashMap.empty

  let (e, t, index, first_map, second_map) = getSystem expr 0 map_first map_second
  let substed = HashMap.empty
  case solveSystem e substed of
    Just solution -> do
      let proof = getProof expr solution first_map
      putStrLn proof
--      putStr $ "proof is here\n"
--      showResult solution
    Nothing -> putStr $ "Expression has no type\n"
--  showResult e
--  putStr $ show expr


