package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.query.ParameterValue;
import cz.cvut.kbss.jopa.query.QueryHolder;

import java.util.*;

/**
 * @author kidney
 */
public class SparqlQueryHolder implements QueryHolder {

    private final Set<String> parameterNames;
    private final List<String> parameters;
    private final List<String> queryParts;
    private final Map<String, ParameterValue> values;

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
    public void setParameter(String parameter, ParameterValue value) {
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
        values.remove(parameter);
    }

    @Override
    public void clearParameters() {
        values.clear();
    }

    @Override
    public String assembleQuery() {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < parameters.size(); i++) {
            sb.append(queryParts.get(i));
            final String paramValue = getParameterValue(parameters.get(i));
            if (paramValue == null) {
                sb.append("?").append(parameters.get(i));
            } else {
                sb.append(paramValue);
            }
        }
        sb.append(queryParts.get(parameters.size()));
        return sb.toString();
    }

    private String getParameterValue(String parameter) {
        return values.containsKey(parameter) ? values.get(parameter).getValue() : null;
    }
}
