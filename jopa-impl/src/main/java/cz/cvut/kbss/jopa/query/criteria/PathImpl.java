package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class PathImpl<X> extends AbstractPathExpression<X> implements Path<X> {

    protected String attributeName;
    protected Attribute<?,X> attribute;

    public PathImpl(Metamodel metamodel, AbstractPathExpression pathSource, Attribute<?,X> attribute, CriteriaBuilder cb) {
        super(attribute.getJavaType(), pathSource, metamodel, cb);
        this.attribute = attribute;
        this.attributeName = attribute.getName();
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        if (this.pathSource != null){
            this.pathSource.setExpressionToQuery(query, parameterFiller);
            query.append("." + attributeName);
        } else {
            query.append(attributeName);
        }
    }

}
