package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.TupleElement;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

import java.util.List;

public class SelectionImpl<T> implements Selection<T>, TupleElement<T> {
    protected final Class<T> type;
    protected ExpressionImpl expression;
    protected String alias;

    public SelectionImpl(Class<T> type, ExpressionImpl expression) {
        this.type = type;
        this.expression = expression;
    }

    protected void setExpression(ExpressionEntityImpl<T> expression){
        this.expression = expression;
    };

    protected void setExpression(ExpressionAttributeImpl<T> expression){
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
    public Selection<T> alias(String name) {
        this.alias = name;
        return this;
    }

    @Override
    public Class<? extends T> getJavaType() {
        return this.type;
    }

    @Override
    public String getAlias() {
        return this.alias;
    }
}
