package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;
import cz.cvut.kbss.jopa.query.QueryHolder;

import java.util.Set;

//TODO PRO - CriteriaQueryHolder implementation
public class CriteriaQueryHolder<T> {

    private final Class<T> type;
    private Selection<? extends T> selection;
    private boolean distinct;

    public CriteriaQueryHolder(Class<T> type) {
        this.type = type;
        this.distinct = false;
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
        final StringBuilder query = new StringBuilder();
        final String alias = type.toString().substring(0,1);
        query.append("SELECT " + (distinct ? "DISTINCT " : ""));

        //TODO PRO difficult select
        query.append(alias);

        query.append(" FROM " + type + " " + alias);
        return query.toString();
    }
}
