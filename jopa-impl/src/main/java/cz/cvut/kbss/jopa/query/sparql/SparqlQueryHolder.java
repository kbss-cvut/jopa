package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParameter;

import java.util.*;

public class SparqlQueryHolder implements QueryHolder {

    // Original query string
    private final String query;

    private final Map<Parameter<?>, QueryParameter<?>> parameterSet;
    private final Map<Object, QueryParameter<?>> identifiersToParameters;
    // These parameters are in order matching the query parts and can appear multiple times in the list
    private final List<QueryParameter<?>> parameters;
    private final List<String> queryParts;

    public SparqlQueryHolder(String query, List<String> parts, List<QueryParameter<?>> parameters) {
        this.query = query;
        this.parameters = parameters;
        this.queryParts = parts;
        this.parameterSet = new HashMap<>();
        parameters.forEach(p -> parameterSet.put(p, p));
        this.identifiersToParameters = new HashMap<>(parameterSet.size());
        parameterSet.values().forEach(p -> identifiersToParameters.put(p.getIdentifier(), p));
    }

    @Override
    public String getQuery() {
        return query;
    }

    @Override
    public Set<Parameter<?>> getParameters() {
        return Collections.unmodifiableSet(parameterSet.keySet());
    }

    @Override
    public QueryParameter<?> getParameter(String name) {
        if (!identifiersToParameters.containsKey(name)) {
            throw unknownParameter(name);
        }
        return identifiersToParameters.get(name);
    }

    private IllegalArgumentException unknownParameter(Object p) {
        return new IllegalArgumentException("Parameter '" + p + "' does not exist in this query.");
    }

    @Override
    public Parameter<?> getParameter(int position) {
        if (!identifiersToParameters.containsKey(position)) {
            throw unknownParameter(position);
        }
        return identifiersToParameters.get(position);
    }

    @Override
    public Object getParameterValue(Parameter<?> parameter) {
        assert getInternalParameter(parameter).getValue() != null;
        return getInternalParameter(parameter).getValue().getValue();
    }

    private QueryParameter<?> getInternalParameter(Parameter<?> p) {
        Objects.requireNonNull(p);
        if (!parameterSet.containsKey(p)) {
            throw unknownParameter(p);
        }
        return parameterSet.get(p);
    }

    @Override
    public <T> void setParameter(Parameter<T> parameter, Object value) {
        Objects.requireNonNull(value);
        getInternalParameter(parameter).setValue(value);
    }

    @Override
    public <T> void setParameter(Parameter<T> parameter, String value, String language) {
        Objects.requireNonNull(value);
        getInternalParameter(parameter).setValue(value, language);
    }

    @Override
    public void clearParameter(Parameter<?> parameter) {
        getInternalParameter(parameter).resetValue();
    }

    @Override
    public void clearParameters() {
        parameterSet.values().forEach(QueryParameter::resetValue);
    }

    @Override
    public String assembleQuery() {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < parameters.size(); i++) {
            sb.append(queryParts.get(i));
            final String paramValue = parameters.get(i).getValue().getQueryString();
            sb.append(paramValue);
        }
        if (queryParts.size() > parameters.size()) {
            sb.append(queryParts.get(parameters.size()));
        }
        return sb.toString();
    }

    @Override
    public String toString() {
        return query;
    }
}
