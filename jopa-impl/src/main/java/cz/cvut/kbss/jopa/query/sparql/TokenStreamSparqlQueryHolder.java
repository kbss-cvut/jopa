package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.loader.SparqlAssemblyModifier;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.TokenStreamRewriter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class TokenStreamSparqlQueryHolder implements QueryHolder {

    private final String query;
    private final QueryAttributes queryAttributes;
    private final TokenStream tokens;

    private final Map<Parameter<?>, TokenQueryParameter<?>> parameterSet;
    private final Map<Object, TokenQueryParameter<?>> identifiersToParameters;

    private SparqlAssemblyModifier assemblyModifier;

    private int offset = 0;

    private int limit = Integer.MAX_VALUE;

    TokenStreamSparqlQueryHolder(String query, QueryAttributes attributes, List<TokenQueryParameter<?>> parameters,
                                 TokenStream tokens) {
        this.query = query;
        this.queryAttributes = attributes;
        this.tokens = tokens;
        this.parameterSet = new LinkedHashMap<>();
        parameters.forEach(p -> parameterSet.put(p, p));
        this.identifiersToParameters = new HashMap<>(parameterSet.size());
        parameterSet.values().forEach(p -> identifiersToParameters.put(p.getIdentifier(), p));
    }

    public void setAssemblyModifier(SparqlAssemblyModifier modifier) {
        this.assemblyModifier = modifier;
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
    public Set<TokenQueryParameter<?>> getQueryParameters() {
        return Set.copyOf(parameterSet.values());
    }

    @Override
    public List<TokenQueryParameter<?>> getProjectedQueryParameters() {
        return parameterSet.values().stream().filter(QueryParameter::isProjected).collect(Collectors.toList());
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
        if (queryAttributes.hasOffset()) {
            throw new IllegalStateException("Query already contains an OFFSET clause.");
        }
        this.offset = startPosition;
    }

    @Override
    public int getFirstResult() {
        return offset;
    }

    public boolean hasOffset() {
        return queryAttributes.hasOffset() || offset != 0;
    }

    @Override
    public void setMaxResults(int maxResults) {
        if (queryAttributes.hasLimit()) {
            throw new IllegalStateException("Query already contains a LIMIT clause.");
        }
        this.limit = maxResults;
    }

    @Override
    public int getMaxResults() {
        return limit;
    }

    public boolean hasLimit() {
        return queryAttributes.hasLimit() || limit != Integer.MAX_VALUE;
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
        final Set<QueryParameter<?>> projectedParams = new LinkedHashSet<>();
        final TokenStreamRewriter rewriter = new TokenStreamRewriter(tokens);
        parameterSet.values().forEach(qp -> {
            if (qp.isProjected() && qp.getValue().isSet()) {
                projectedParams.add(qp);
            } else {
                qp.getTokens().forEach(t -> rewriter.replace(t, qp.getValue().getQueryString()));
            }
        });
        if (assemblyModifier != null) {
            assemblyModifier.modify(this, rewriter, queryAttributes);
        }
        final StringBuilder sb = new StringBuilder(rewriter.getText());
        if (limit != Integer.MAX_VALUE) {
            sb.append(" LIMIT ").append(limit);
        }
        if (offset != 0) {
            sb.append(" OFFSET ").append(offset);
        }
        assembleValuesClause(projectedParams).ifPresent(sb::append);
        return sb.toString();
    }

    /**
     * Generates a VALUES clause for query parameters that are set and appear in SELECT projection.
     * <p>
     * TODO Note that the current implementation does not support collection-valued parameters.
     *
     * @param parameters Projected parameters to output into the query as VALUES clause
     * @return VALUES clause, if there were any set parameters
     */
    private static Optional<String> assembleValuesClause(Set<QueryParameter<?>> parameters) {
        if (parameters.isEmpty()) {
            return Optional.empty();
        }
        final StringBuilder variables = new StringBuilder();
        final int tableSize = maxValueCount(parameters);
        final List<List<String>> valueTable = new ArrayList<>(parameters.size());
        for (QueryParameter<?> qp : parameters) {
            if (!variables.isEmpty()) {
                variables.append(' ');
            }
            variables.append(qp.getIdentifierAsQueryString());
            valueTable.add(qp.getValue().toQueryValues(tableSize));
        }
        return Optional.of(" VALUES (" + variables + ") { " + valueTableToString(valueTable, tableSize) + "}");
    }

    private static int maxValueCount(Set<QueryParameter<?>> parameters) {
        return parameters.stream().map(p -> p.getValue().valueCount()).max(Integer::compareTo).orElse(1);
    }

    private static String valueTableToString(List<List<String>> valueTable, int rowSize) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < rowSize; i++) {
            sb.append("( ");
            for (List<String> row : valueTable) {
                sb.append(row.get(i)).append(" ");
            }
            sb.append(") ");
        }
        return sb.toString();
    }

    @Override
    public QueryType getQueryType() {
        return queryAttributes.queryType();
    }

    public QueryAttributes getQueryAttributes() {
        return queryAttributes;
    }
}
