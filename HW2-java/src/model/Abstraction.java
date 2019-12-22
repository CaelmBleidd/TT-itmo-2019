package model;

public class Abstraction extends Expression {
    private Variable argument;
    private Expression expression;

    public Abstraction(Variable argument, Expression expression) {
        this.argument = argument;
        this.expression = expression;
    }

    @Override
    public String toString() {
        return "(\\" + argument.toString() + "." + expression.toString() + ")";
    }

    @Override
    public int hashCode() {
        return argument.hashCode() * 31 + expression.hashCode() * 17;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Abstraction) {
            return expression.equals(((Abstraction) other).expression);
        } else {
            return false;
        }
    }

    public Variable getArgument() {
        return argument;
    }

    public void setArgument(Variable argument) {
        this.argument = argument;
    }

    public Expression getExpression() {
        return expression;
    }

    public void setExpression(Expression expression) {
        this.expression = expression;
    }
}

