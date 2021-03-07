package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.TupleElement;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

import java.util.List;

public class SelectionImpl<X> implements Selection<X>, TupleElement<X> {
    protected final Class<X> type;
    protected ExpressionImpl expression;
    protected String alias;

    public SelectionImpl(Class<X> type, ExpressionImpl expression) {
        this.type = type;
        this.expression = expression;
    }

    protected void setExpression(ExpressionEntityImpl<X> expression){
        this.expression = expression;
    };

    protected void setExpression(ExpressionAttributeImpl<X> expression){
        this.expression = expression;
    };

    @Override
    public boolean isCompoundedSelection() {
        return false;
    }

    @Override
    public List<Selection<?>> getCompoundedSelectionItems() {
        return null;
    }

    @Override
    public Selection<X> alias(String name) {
        this.alias = name;
        return this;
    }

    @Override
    public Class<? extends X> getJavaType() {
        return this.type;
    }

    @Override
    public String getAlias() {
        return this.alias;
    }
}
