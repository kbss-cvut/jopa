package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryType;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStreamRewriter;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.jopa.query.sparql.SparqlQueryHolder.SPARQL_LIMIT;
import static cz.cvut.kbss.jopa.query.sparql.SparqlQueryHolder.SPARQL_OFFSET;
import static cz.cvut.kbss.jopa.query.sparql.SparqlQueryHolder.assembleValuesClause;

public class TokenStreamSparqlQueryHolder implements QueryHolder {

    private final String query;
    private final QueryType queryType;
    private final CommonTokenStream tokens;

    private final Map<Parameter<?>, TokenQueryParameter<?>> parameterSet;
    private final Map<Object, TokenQueryParameter<?>> identifiersToParameters;

    private int offset = 0;

    private int limit = Integer.MAX_VALUE;

    public TokenStreamSparqlQueryHolder(String query, QueryType queryType, List<TokenQueryParameter<?>> parameters,
                                        CommonTokenStream tokens) {
        this.query = query;
        this.queryType = queryType;
        this.tokens = tokens;
        this.parameterSet = new LinkedHashMap<>();
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
        final Set<QueryParameter<?>> projectedParams = new LinkedHashSet<>();
        final TokenStreamRewriter rewriter = new TokenStreamRewriter(tokens);
        parameterSet.values().forEach(qp -> {
            if (qp.isProjected() && qp.getValue().isSet()) {
                projectedParams.add(qp);
            } else {
                qp.getTokens().forEach(t -> rewriter.replace(t, qp.getValue().getQueryString()));
            }
        });
        final StringBuilder sb = new StringBuilder(rewriter.getText());
        if (limit != Integer.MAX_VALUE) {
            sb.append(SPARQL_LIMIT).append(limit);
        }
        if (offset != 0) {
            sb.append(SPARQL_OFFSET).append(offset);
        }
        assembleValuesClause(projectedParams).ifPresent(sb::append);
        return sb.toString();
    }

    @Override
    public QueryType getQueryType() {
        return queryType;
    }
}
