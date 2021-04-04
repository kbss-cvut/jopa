package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.*;
import java.util.List;

public class CriteriaQueryHolder<T> {

    protected final Class<T> resultType;
    protected Selection<? extends T> selection;
    private boolean distinct;
    protected Root<?> root;
    protected Predicate where;
    protected List<Order> orderBy;
    protected List<Expression<?>> groupBy;
    protected Predicate having;

    public CriteriaQueryHolder(Class<T> resultType) {
        this.resultType = resultType;
        this.distinct = false;
    }

    public Class<T> getResultType() {
        return resultType;
    }

    public Predicate getHaving() {
        return having;
    }

    public void setHaving(Predicate having) {
        this.having = having;
    }

    public Root<?> getRoot() {
        return root;
    }

    public void setRoot(Root<?> root) {
        this.root = root;
    }

    public Predicate getWhere() {
        return where;
    }

    public void setWhere(Predicate where) {
        this.where = where;
    }

    public List<Expression<?>> getGroupBy() {
        return groupBy;
    }

    public void setGroupBy(List<Expression<?>> groupBy) {
        this.groupBy = groupBy;
    }

    public List<Order> getOrderBy() {
        return orderBy;
    }

    public void setOrderBy(List<Order> orderBy) {
        this.orderBy = orderBy;
    }

    public Selection<? extends T> getSelection() {
        return selection;
    }

    public void setSelection(Selection<? extends T> selection) {
        this.selection = selection;
    }

    public boolean isDistinct() {
        return distinct;
    }

    public void setDistinct(boolean distinct) {
        this.distinct = distinct;
    }

}
