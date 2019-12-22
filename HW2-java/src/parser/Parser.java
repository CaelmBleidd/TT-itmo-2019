package parser;

import model.Abstraction;
import model.Application;
import model.Expression;
import model.Variable;

public class Parser {
    public Expression parse(String input) {
        String line = input.trim();
        Result res = getApplication(line, 0);
        return res.getAcc();
    }

    private Result getApplication(String line, int left) {
        Result current = getBracket(line, left);
        Expression acc = current.acc;
        while (true) {
            if (current.left == line.length()) {
                return current;
            }
            char character = line.charAt(current.left);
            if (character == ')') {
                return current;
            }
            current.left = skipWhiteSpace(current.left, line);
            Result right = getBracket(line, current.left);
            acc = new Application(acc, right.acc);
            current = new Result(acc, right.left);
        }
    }


    private Result getBracket(String line, int begin) {
        int nextBegin = skipWhiteSpace(begin, line);
        int zeroChar = line.charAt(nextBegin);
        if (zeroChar == '\\') {
            Result argument = getVariable(line, skipWhiteSpace(nextBegin + 1, line));
            int nextTerm = skipWhiteSpace(argument.left, line);
            Result expression = getApplication(line, skipWhiteSpace(nextTerm + 1, line));
            Abstraction result = new Abstraction((Variable) argument.acc, expression.acc);
            return new Result(result, expression.left);
        }

        if (zeroChar == '(') {
            Result r = getApplication(line, skipWhiteSpace(nextBegin + 1, line));
            r.left = skipWhiteSpace(r.left, line);
            if (line.charAt(r.left) == ')') {
                r.left++;
            }
            if (r.left < line.length() - 1) {
                r.left = skipWhiteSpace(r.left, line);
            }
            return r;
        }

        return getVariable(line, nextBegin);
    }

    private int skipWhiteSpace(int next, String line) {
        while (Character.isWhitespace(line.charAt(next))) {
            ++next;
        }
        return next;
    }

    private Result getVariable(String line, int begin) {
        StringBuilder stringBuilder = new StringBuilder();
        int i = 0;
        while (begin + i < line.length() &&
               (Character.isLetter(line.charAt(begin + i)) ||
               Character.isDigit(line.charAt(begin + i)) ||
               line.charAt(begin + i) == '\'')) {
            stringBuilder.append(line.charAt(begin + i));
            i++;
        }
        Variable result = new Variable(stringBuilder.toString());
        int next = begin + i;
        if (next < line.length() - 1) {
            next = skipWhiteSpace(next, line);
        }
        return new Result(result, next);
    }

    private static class Result {
        private Expression acc;
        private Integer left;

        public Result(Expression acc, Integer left) {
            this.acc = acc;
            this.left = left;
        }

        public Expression getAcc() {
            return acc;
        }

        public void setAcc(Expression acc) {
            this.acc = acc;
        }

        public Integer getLeft() {
            return left;
        }

        public void setLeft(Integer left) {
            this.left = left;
        }
    }
}

