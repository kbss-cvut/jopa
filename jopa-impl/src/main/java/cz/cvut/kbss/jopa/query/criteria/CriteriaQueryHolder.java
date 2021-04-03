package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.criteria.Order;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;

import java.util.Collections;
import java.util.List;
import java.util.Set;

public class CriteriaQueryHolder<T> {

    protected final Class<T> resultType;
    protected SelectionImpl<? extends T> selection;
    private boolean distinct;
    protected AbstractExpression<Boolean> where;
    protected List<Order> orderBy;
    protected RootImpl<?> root;

    public CriteriaQueryHolder(Class<T> resultType) {
        this.resultType = resultType;
        this.distinct = false;
    }

    public RootImpl<?> getRoot() {
        return root;
    }

    public void setRoot(RootImpl<?> root) {
        this.root = root;
    }

    public AbstractExpression<Boolean> getWhere() {
        return where;
    }

    public void setWhere(AbstractExpression<Boolean> where) {
        this.where = where;
    }

    public List<Order> getOrderBy() {
        if (orderBy == null) return Collections.emptyList();
        return orderBy;
    }

    public void setOrderBy(List<Order> orderBy) {
        this.orderBy = orderBy;
    }

    public SelectionImpl<? extends T> getSelection() {
        return selection;
    }

    public void setSelection(SelectionImpl<? extends T> selection) {
        this.selection = selection;
    }

    public boolean isDistinct() {
        return distinct;
    }

    public void setDistinct(boolean distinct) {
        this.distinct = distinct;
    }

    public Set<Parameter<?>> getParameters() {
        return null;
    }

    public Parameter<?> getParameter(String name) {
        return null;
    }

    public Parameter<?> getParameter(int position) {
        return null;
    }

    public Object getParameterValue(Parameter<?> parameter) {
        return null;
    }

    public <T> void setParameter(Parameter<T> parameter, Object value) {

    }

    public <T> void setParameter(Parameter<T> parameter, String value, String language) {

    }

    public <T> void setUntypedParameter(Parameter<T> parameter, Object value) {

    }

    public void clearParameter(Parameter<?> parameter) {

    }

    public void clearParameters() {

    }
}
