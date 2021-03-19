package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;

public class ParameterExpressionImpl<T> extends AbstractExpression<T> implements ParameterExpression<T> {
    private final String name;

    public ParameterExpressionImpl(Class<T> type, String name) {
        super(type);
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Integer getPosition() {
        return null;
    }

    @Override
    public Class<T> getParameterType() {
        return type;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query) {

    }
}
