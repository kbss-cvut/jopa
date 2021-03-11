package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.exception.MissingChildExpressionException;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;

public class ExpressionCountImpl<Y> extends ExpressionImpl<Y> {
    public ExpressionCountImpl(Class<Y> type, ExpressionImpl expression) {
        super(type, expression);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query) {
        try {
            query.append("COUNT(");
            this.expression.setExpressionToQuery(query);
            query.append(") ");
        } catch (NullPointerException e){
            throw new MissingChildExpressionException("Expression representing COUNT method is missing child expression.");
        }
    }
}
