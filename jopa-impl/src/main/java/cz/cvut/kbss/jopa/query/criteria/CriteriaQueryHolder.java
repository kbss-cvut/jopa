package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ManagedType;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Order;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

import java.util.List;
import java.util.Set;

//TODO PRO - CriteriaQueryHolder implementation
public class CriteriaQueryHolder<T> {

    protected final Class<T> resultType;
    protected final ManagedType<T> managedResultType;
    protected Selection<? extends T> selection;
    private boolean distinct;
    protected Expression<Boolean> where;
    protected List<Order> orderBy;
    protected Root<?> root;

    public CriteriaQueryHolder(EntityType<T> managedResultType, Class<T> resultType) {
        this.managedResultType = managedResultType;
        this.resultType = resultType;
        this.distinct = false;
    }

    public Root<?> getRoot() {
        return root;
    }

    public void setRoot(Root<?> root) {
        this.root = root;
    }

    public Expression<Boolean> getWhere() {
        return where;
    }

    public void setWhere(Expression<Boolean> where) {
        this.where = where;
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
