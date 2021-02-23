package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Order;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;

import java.util.List;
import java.util.Set;

//TODO PRO - CriteriaQueryHolder implementation
public class CriteriaQueryHolder<T> {

    protected final Class<T> resultType;
    protected final EntityType<T> entityMetamodel;
    protected Selection<? extends T> selection;
    private boolean distinct;
    protected Expression<Boolean> where;
    protected List<Order> orderBy;

    public CriteriaQueryHolder(EntityType<T> entityMetamodel, Class<T> resultType) {
        this.entityMetamodel = entityMetamodel;
        this.resultType = resultType;
        this.distinct = false;
        this.selection = new ExpressionEntityImpl<>(resultType);
    }

    public Selection<T> getSelection() {
        return (Selection<T>) selection;
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

    public String assembleSoqlQuery() {
//        final StringBuilder query = new StringBuilder();
//        query.append("SELECT " + (distinct ? "DISTINCT " : ""));
//
//        //TODO PRO difficult select
//        selection.
//        query.append(alias);
//
//        query.append(" FROM " + resultType + " " + alias);
//        return query.toString();
        return "0";
    }
}
