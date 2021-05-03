package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public class ExpressionAttributeImpl<X> extends AbstractPathExpression<X> {

    protected String attributeName;
    protected Attribute<?,X> attribute;

    public ExpressionAttributeImpl(Class<X> type, AbstractPathExpression pathSource, Metamodel metamodel, Attribute<?,X> attribute, CriteriaBuilder cb) {
        super(type, pathSource, metamodel, cb);
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
