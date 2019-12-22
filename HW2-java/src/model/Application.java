package model;


public class Application extends Expression {
    private Expression left;
    private Expression right;

    @Override
    public String toString() {
        return "(" + left.toString() + " " + right.toString() + ")";
    }

    @Override
    public int hashCode() {
        return left.hashCode() * 37 + right.hashCode() * 17;
    }

    public Application(Expression left, Expression right) {
        this.left = left;
        this.right = right;
    }

    public Expression getLeft() {
        return left;
    }

    public void setLeft(Expression left) {
        this.left = left;
    }

    public Expression getRight() {
        return right;
    }

    public void setRight(Expression right) {
        this.right = right;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Application) {
            return left.equals(((Application) other).left) && right.equals(((Application) other).right);
        } else {
            return false;
        }
    }
}
