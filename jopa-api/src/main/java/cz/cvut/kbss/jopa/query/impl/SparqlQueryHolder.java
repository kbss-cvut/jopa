package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.query.QueryHolder;

import java.util.*;

/**
 * @author kidney
 */
public class SparqlQueryHolder implements QueryHolder {

    private final Set<String> parameterNames;
    private final List<String> parameters;
    private final List<String> queryParts;
    private final Map<String, Object> values;

    public SparqlQueryHolder(List<String> parts, List<String> parameters) {
        this.parameters = parameters;
        this.queryParts = parts;
        this.parameterNames = new HashSet<>(parameters);
        this.values = new HashMap<>(parameterNames.size());
    }

    @Override
    public Collection<String> getParameters() {
        return Collections.unmodifiableSet(parameterNames);
    }

    @Override
    public void setParameter(String parameter, Object value) {
        Objects.requireNonNull(parameter);
        Objects.requireNonNull(value);
        if (!parameterNames.contains(parameter)) {
            throw new IllegalArgumentException(
                    "Unknown parameter name " + parameter + ". There is no such variable in the query.");
        }
        values.put(parameter, value);
    }

    @Override
    public void clearParameter(String parameter) {

    }

    @Override
    public void clearParameters() {

    }

    @Override
    public String assembleQuery() {
        return null;
    }
}
