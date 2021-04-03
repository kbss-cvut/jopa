package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Order;

public class OrderImpl implements Order {
    private final Expression<?> expression;
    private boolean ascending;

    public OrderImpl(Expression<?> expression, boolean ascending) {
        this.expression = expression;
        this.ascending = ascending;
    }

    public OrderImpl(Expression<?> expression) {
        this.expression = expression;
        this.ascending = true;
    }

    @Override
    public Expression<?> getExpression() {
        return expression;
    }

    @Override
    public boolean isAscending() {
        return ascending;
    }

    @Override
    public Order reverse() {
        ascending = !ascending;
        return this;
    }
}
