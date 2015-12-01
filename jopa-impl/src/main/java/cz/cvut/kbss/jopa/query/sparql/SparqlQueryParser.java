package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.exception.QueryParserException;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryParser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SparqlQueryParser implements QueryParser {

    private String query;

    private Map<Object, QueryParameter<?>> uniqueParams;
    private Integer positionalCounter;

    List<String> queryParts;
    List<QueryParameter<?>> parameters;
    private boolean inParam;
    private boolean inSQString; // In apostrophe string (')
    private boolean inDQString; // In double-quoted string (")
    private int lastParamEndIndex;
    private int paramStartIndex;
    ParamType currentParamType;

    private enum ParamType {
        POSITIONAL, NAMED
    }

    @Override
    public SparqlQueryHolder parseQuery(String query) {
        this.query = query;
        this.queryParts = new ArrayList<>();
        this.uniqueParams = new HashMap<>();
        this.positionalCounter = 1;
        this.parameters = new ArrayList<>();
        this.inSQString = false;
        // In double-quoted string
        this.inDQString = false;
        this.inParam = false;
        this.lastParamEndIndex = 0;
        this.paramStartIndex = 0;
        this.currentParamType = null;
        for (int i = 0; i < query.length(); i++) {
            final char c = query.charAt(i);
            switch (c) {
                case '\'':
                    inSQString = !inSQString;
                    break;
                case '"':
                    inDQString = !inDQString;
                    break;
                case '$':
                    parameterStart(i, ParamType.POSITIONAL);
                    break;
                case '?':
                    parameterStart(i, ParamType.NAMED);
                    break;
                // TODO Take a look at some existing SPARQL parsers and maybe use them instead of this simplified version
                case '<':
                case '>':
                case ',':
                case '\n':
                case ')':
                case ' ':
                case '.':
                case ';':
                    if (inParam) {
                        parameterEnd(i);
                    }
                    break;
                default:
                    break;
            }
        }
        queryParts.add(query.substring(lastParamEndIndex));
        return new SparqlQueryHolder(query, queryParts, parameters);
    }

    private void parameterStart(int index, ParamType paramType) {
        if (!inSQString && !inDQString) {
            queryParts.add(query.substring(lastParamEndIndex, index));
            paramStartIndex = index + 1;
            inParam = true;
            this.currentParamType = paramType;
        }
    }

    private void parameterEnd(int index) {
        this.lastParamEndIndex = index;
        this.inParam = false;
        final String param = query.substring(paramStartIndex, index);
        parameters.add(resolveParamIdentification(param));
    }

    private QueryParameter<?> resolveParamIdentification(String identification) {
        final QueryParameter<?> queryParameter;
        if (identification.isEmpty()) {
            if (currentParamType == ParamType.POSITIONAL) {
                queryParameter = getQueryParameter(positionalCounter++);
            } else {
                throw new QueryParserException("Missing parameter name in query " + query);
            }
        } else {
            if (currentParamType == ParamType.POSITIONAL) {
                try {
                    Integer position = Integer.parseInt(identification);
                    positionalCounter++;
                    queryParameter = getQueryParameter(position);
                } catch (NumberFormatException e) {
                    throw new QueryParserException(identification + " is not a valid parameter position.", e);
                }
            } else {
                queryParameter = getQueryParameter(identification);
            }
        }
        return queryParameter;
    }

    private QueryParameter<?> getQueryParameter(String name) {
        // We want to reuse the param instances, so that changes to them apply throughout the whole query
        if (!uniqueParams.containsKey(name)) {
            uniqueParams.put(name, new QueryParameter<>(name));
        }
        return uniqueParams.get(name);
    }

    private QueryParameter<?> getQueryParameter(Integer position) {
        // We want to reuse the param instances, so that changes to them apply throughout the whole query
        if (uniqueParams.containsKey(position)) {
            throw new QueryParserException("Parameter with position " + position + " already found in query " + query);
        }
        final QueryParameter<?> qp = new QueryParameter<>(position);
        uniqueParams.put(position, qp);
        return qp;
    }
}
