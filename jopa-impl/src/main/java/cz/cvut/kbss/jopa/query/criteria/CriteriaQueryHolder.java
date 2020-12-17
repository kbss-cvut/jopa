package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryHolder;

import java.util.Set;

public class CriteriaQueryHolder implements QueryHolder {
    @Override
    public String getQuery() {
        return null;
    }

    @Override
    public Set<Parameter<?>> getParameters() {
        return null;
    }

    @Override
    public Parameter<?> getParameter(String name) {
        return null;
    }

    @Override
    public Parameter<?> getParameter(int position) {
        return null;
    }

    @Override
    public Object getParameterValue(Parameter<?> parameter) {
        return null;
    }

    @Override
    public <T> void setParameter(Parameter<T> parameter, Object value) {

    }

    @Override
    public <T> void setParameter(Parameter<T> parameter, String value, String language) {

    }

    @Override
    public <T> void setUntypedParameter(Parameter<T> parameter, Object value) {

    }

    @Override
    public void setFirstResult(int startPosition) {

    }

    @Override
    public int getFirstResult() {
        return 0;
    }

    @Override
    public void setMaxResults(int maxResults) {

    }

    @Override
    public int getMaxResults() {
        return 0;
    }

    @Override
    public void clearParameter(Parameter<?> parameter) {

    }

    @Override
    public void clearParameters() {

    }

    @Override
    public String assembleQuery() {
        return null;
    }
}
