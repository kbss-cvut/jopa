package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

public class PathImpl<X> extends AbstractPathExpression<X> implements Path<X> {

    public PathImpl(Metamodel metamodel, AbstractPathExpression<X> expression, Class<X> type, CriteriaFactory factory) {
        super(type, expression, metamodel, factory);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        this.pathSource.setExpressionToQuery(query, parameterFiller);
    }
}
