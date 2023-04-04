/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParameter;

import java.util.*;

public class SparqlQueryHolder implements QueryHolder {

    private static final String SPARQL_LIMIT = " LIMIT ";
    private static final String SPARQL_OFFSET = " OFFSET ";

    // Original query string
    private final String query;

    private final Map<Parameter<?>, QueryParameter<?>> parameterSet;
    private final Map<Object, QueryParameter<?>> identifiersToParameters;
    // These parameters are in order matching the query parts and can appear multiple times in the list
    private final List<QueryParameter<?>> parameters;
    private final List<String> queryParts;

    private int offset = 0;

    private int limit = Integer.MAX_VALUE;

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
    public boolean hasParameter(String name) {
        return identifiersToParameters.containsKey(name);
    }

    @Override
    public boolean hasParameter(int position) {
        return identifiersToParameters.containsKey(position);
    }

    @Override
    public QueryParameter<?> getParameter(String name) {
        if (!hasParameter(name)) {
            throw unknownParameter(name);
        }
        return identifiersToParameters.get(name);
    }

    private static IllegalArgumentException unknownParameter(Object p) {
        return new IllegalArgumentException("Parameter '" + p + "' does not exist in this query.");
    }

    @Override
    public Parameter<?> getParameter(int position) {
        if (!hasParameter(position)) {
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
    public <T> void setUntypedParameter(Parameter<T> parameter, Object value) {
        Objects.requireNonNull(value);
        getInternalParameter(parameter).setUntypedValue(value);
    }

    @Override
    public void setFirstResult(int startPosition) {
        this.offset = startPosition;
    }

    @Override
    public int getFirstResult() {
        return offset;
    }

    @Override
    public void setMaxResults(int maxResults) {
        this.limit = maxResults;
    }

    @Override
    public int getMaxResults() {
        return limit;
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
        final Set<QueryParameter<?>> projectedParams = new LinkedHashSet<>();
        for (int i = 0; i < parameters.size(); i++) {
            sb.append(queryParts.get(i));
            final QueryParameter<?> qp = parameters.get(i);
            if (qp.isProjected() && qp.getValue().isSet()) {
                projectedParams.add(qp);
                sb.append(qp.getIdentifierAsQueryString());
            } else {
                sb.append(qp.getValue().getQueryString());
            }
        }
        if (queryParts.size() > parameters.size()) {
            sb.append(queryParts.get(parameters.size()));
        }
        if (limit != Integer.MAX_VALUE) {
            sb.append(SPARQL_LIMIT).append(limit);
        }
        if (offset != 0) {
            sb.append(SPARQL_OFFSET).append(offset);
        }
        assembleValuesClause(projectedParams).ifPresent(sb::append);
        return sb.toString();
    }

    /**
     * Generates a VALUES clause for query parameters that are set and appear in SELECT projection.
     * <p>
     * TODO Note that the current implementation does not support collection-valued parameters.
     *
     * @param parameters Projected parameters to output into query as VALUES clause
     * @return VALUES clause, if there were any set parameters
     */
    private Optional<String> assembleValuesClause(Set<QueryParameter<?>> parameters) {
        if (parameters.isEmpty()) {
            return Optional.empty();
        }
        final StringBuilder variables = new StringBuilder();
        final StringBuilder data = new StringBuilder();
        for (QueryParameter<?> qp : parameters) {
            if (variables.length() > 0) {
                variables.append(' ');
            }
            variables.append(qp.getIdentifierAsQueryString());
            data.append('(').append(qp.getValue().getQueryString()).append(')');
        }
        return Optional.of(" VALUES (" + variables + ") {" + data + "}");
    }

    @Override
    public String toString() {
        return assembleQuery();
    }
}
