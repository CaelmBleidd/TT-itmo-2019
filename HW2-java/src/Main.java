import model.Abstraction;
import model.Application;
import model.Expression;
import model.Variable;
import parser.Parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Objects;

/*
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
 */

public class Main {

    public static Integer index = 0;

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

        String line = br.readLine();

        int m, k;
        m = Integer.parseInt(line.split(" ")[0]);
        k = Integer.parseInt(line.split(" ")[1]);

        Expression expression = new Parser().parse(br.readLine());
        System.out.println(expression.toString());
        for (int i = 1; i <= m; ++i) {
            Application redux = findBetaRedux(expression);
            if (redux == null) {
                if ((i - 1) % k != 0) {
                    System.out.println(expression.toString());
                }
                break;
            }

            Expression right = Objects.requireNonNull(redux).getRight();
            Expression left = memoization(((Abstraction) redux.getLeft()).getExpression(), new HashMap<>());

            replaceBold(left, new HashMap<>());
            Expression reduced = substitute(left, ((Abstraction) redux.getLeft()).getArgument(), right);

            expression = reduct(expression, redux, reduced);
            if (i % k == 0) {
                System.out.println(expression.toString());
            }
        }
    }

    private static Expression reduct(Expression expression, Application redux, Expression reduced) {
        if (expression instanceof Variable) {
            return expression;
        }

        if (expression instanceof Application) {
            if (redux == expression) {
                return reduced;
            }

            ((Application) expression).setLeft(reduct(((Application) expression).getLeft(), redux, reduced));
            ((Application) expression).setRight(reduct(((Application) expression).getRight(), redux, reduced));
            return expression;
        }

        if (expression instanceof Abstraction) {
            ((Abstraction) expression).setExpression(reduct(((Abstraction) expression).getExpression(),
                                                            redux, reduced));
            return expression;
        }
        return null;
    }


    private static Expression substitute(Expression left, Variable argument, Expression right) {
        if (left instanceof Variable) {
            if (left.equals(argument)) {
                return right;
            } else {
                return left;
            }
        }

        if (left instanceof Application) {
            Expression leftResult = substitute(((Application) left).getLeft(), argument, right);
            Expression rightResult = substitute(((Application) left).getRight(), argument, right);
            ((Application) left).setLeft(leftResult);
            ((Application) left).setRight(rightResult);
            return left;
        }

        if (left instanceof Abstraction) {
            Expression result = substitute(((Abstraction) left).getExpression(), argument, right);
            ((Abstraction) left).setExpression(result);
            return left;
        }
        return null;
    }


    private static void replaceBold(Expression expression, HashMap<String, String> stringStringHashMap) {
        if (expression instanceof Variable) {
            String name = stringStringHashMap.get(((Variable) expression).getName());
            if (name == null) {
                name = ((Variable) expression).getName();
            }
            ((Variable) expression).setName(name);
            return;
        }

        if (expression instanceof Application) {
            replaceBold(((Application) expression).getRight(), stringStringHashMap);
            replaceBold(((Application) expression).getLeft(), stringStringHashMap);
            return;
        }

        if (expression instanceof Abstraction) {
            String newName = "v" + index++;
            if (stringStringHashMap.containsKey(((Abstraction) expression).getArgument().getName())) {
                String oldName = stringStringHashMap.get(((Abstraction) expression).getArgument().getName());
                stringStringHashMap.put(((Abstraction) expression).getArgument().getName(), newName);
                replaceBold(((Abstraction) expression).getExpression(), stringStringHashMap);
                stringStringHashMap.put(((Abstraction) expression).getArgument().getName(), oldName);
            } else {
                stringStringHashMap.put(((Abstraction) expression).getArgument().getName(), newName);
                replaceBold(((Abstraction) expression).getExpression(), stringStringHashMap);
                stringStringHashMap.remove(((Abstraction) expression).getArgument().getName());
            }
            ((Abstraction) expression).getArgument().setName(newName);
        }
    }


    private static Expression createNewObject(Expression expression) {
        if (expression instanceof Variable) {
            return new Variable(((Variable) expression).getName());
        }

        if (expression instanceof Application) {
            return new Application(createNewObject(((Application) expression).getLeft()),
                                   createNewObject(((Application) expression).getRight()));
        }

        if (expression instanceof Abstraction) {
            return new Abstraction((Variable) createNewObject(((Abstraction) expression).getArgument()),
                                   createNewObject(((Abstraction) expression).getExpression()));
        }
        return null;
    }

    private static Expression memoization(Expression expression, HashMap<Expression, Expression> expressionExpressionHashMap) {
        Expression value = expressionExpressionHashMap.get(expression);
        if (value != null) {
            return value;
        }

        if (expression instanceof Variable) {
            Variable variable = new Variable(((Variable) expression).getName());
            expressionExpressionHashMap.put(expression, variable);
        }

        if (expression instanceof Application) {
            Application application = new Application(createNewObject(((Application) expression).getLeft()),
                                                      createNewObject(((Application) expression).getRight()));
            expressionExpressionHashMap.put(expression, application);
        }

        if (expression instanceof Abstraction) {
            Abstraction abstraction = new Abstraction((Variable) createNewObject(((Abstraction) expression).getArgument()),
                                                      createNewObject(((Abstraction) expression).getExpression()));
            expressionExpressionHashMap.put(expression, abstraction);
        }
        return expressionExpressionHashMap.get(expression);
    }


    /*
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
     */
    private static Application findBetaRedux(Expression expression) {
        if (expression instanceof Variable) {
            return null;
        }

        if (expression instanceof Application) {
            Application tmp = (Application) expression;
            if (tmp.getLeft() instanceof Abstraction) {
                return (Application) expression;
            } else {
                Application result = findBetaRedux(tmp.getLeft());
                if (result == null) {
                    return findBetaRedux(tmp.getRight());
                } else {
                    return result;
                }
            }
        }

        if (expression instanceof Abstraction) {
            return findBetaRedux(((Abstraction) expression).getExpression());
        }

        return null;
    }
}

