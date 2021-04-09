package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;

import java.util.Objects;

public class ParameterExpressionImpl<T> extends AbstractExpression<T> implements ParameterExpression<T> {
    private String name;

    public ParameterExpressionImpl(Class<T> type, String name) {
        super(type);
        this.name = name;
    }

    public void setNameIfUnnamed(String name) {
        if (this.name == null) this.name = name;
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
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(parameterFiller.registerParameter(this));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || !Parameter.class.isAssignableFrom(o.getClass())) {
            return false;
        }
        Parameter<?> that = (Parameter<?>) o;
        return Objects.equals(getName(), that.getName());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getName(), getPosition());
    }
}
