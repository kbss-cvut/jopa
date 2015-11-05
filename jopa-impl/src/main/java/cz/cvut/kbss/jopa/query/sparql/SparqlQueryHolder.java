package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValue;

import java.util.*;

public class SparqlQueryHolder implements QueryHolder {

    private final Set<Parameter<?>> parameterSet;
    // These parameters are in order matching the query parts and can appear multiple times in the list
    private final List<QueryParameter<?>> parameters;
    private final List<String> queryParts;

    public SparqlQueryHolder(List<String> parts, List<QueryParameter<?>> parameters) {
        this.parameters = parameters;
        this.queryParts = parts;
        this.parameterSet = new HashSet<>(parameters);
    }

    @Override
    public Collection<Parameter<?>> getParameters() {
        return Collections.unmodifiableSet(parameterSet);
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
    public <T> void setParameter(Parameter<T> parameter, ParameterValue value) {

    }

    @Override
    public void clearParameter(Parameter<?> parameter) {

    }

    @Override
    public void clearParameters() {

    }

    @Override
    public String assembleQuery() {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < parameters.size(); i++) {
            sb.append(queryParts.get(i));
            final String paramValue = parameters.get(i).getValue().getQueryString();
            sb.append(paramValue);
        }
        sb.append(queryParts.get(parameters.size()));
        return sb.toString();
    }

}
