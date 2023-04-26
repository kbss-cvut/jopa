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

import cz.cvut.kbss.jopa.exception.QueryParserException;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A simplified SPARQL query parser.
 * <p>
 * This implementation does not use any AST tree-based query string parsing as its only purpose is to split the query
 * into chunks delimited by variable occurrences, so that the variables can be bound using parameters in the query API.
 * <p>
 * More diligent query parsing is left to the engine used to execute the resulting query.
 */
public class SparqlQueryParser implements QueryParser {

    private static final String SELECT = "SELECT";
    private static final String WHERE = "WHERE";

    private final ParameterValueFactory parameterValueFactory;

    private String query;

    private Map<Object, QueryParameter<?>> uniqueParams;
    private Integer positionalCounter;

    private List<String> queryParts;
    private List<QueryParameter<?>> parameters;
    private boolean inParam;
    private boolean inSQString; // In apostrophe string (')
    private boolean inDQString; // In double-quoted string (")
    private int lastParamEndIndex;
    private int paramStartIndex;
    private ParamType currentParamType;
    private StringBuilder currentWord;
    private boolean inProjection;

    public SparqlQueryParser(ParameterValueFactory parameterValueFactory) {
        this.parameterValueFactory = parameterValueFactory;
    }

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
        this.currentWord = new StringBuilder();
        int i;
        for (i = 0; i < query.length(); i++) {
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
                    if (inParam) {
                        parameterEnd(i);    // Property path zero or one
                    } else {
                        parameterStart(i, ParamType.NAMED);
                    }
                    break;
                case '<':
                case '>':
                case ',':
                case '\n':
                case '\r':
                case ')':
                case ' ':
                case '.':
                case ';':
                case '{':
                case '}':
                case '[':
                case ']':
                case '+':
                case '*':
                case '/':
                case '|':
                    if (inParam) {
                        parameterEnd(i);
                    }
                    wordEnd();
                    break;
                default:
                    currentWord.append(c);
                    break;
            }
        }
        if (inParam) {
            parameterEnd(i);
        } else {
            queryParts.add(query.substring(lastParamEndIndex));
        }
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
            final QueryParameter<?> qp = new QueryParameter<>(name, parameterValueFactory);
            qp.setProjected(inProjection);
            uniqueParams.put(name, qp);
        }
        return uniqueParams.get(name);
    }

    private QueryParameter<?> getQueryParameter(Integer position) {
        if (uniqueParams.containsKey(position)) {
            throw new QueryParserException("Parameter with position " + position + " already found in query " + query);
        }
        final QueryParameter<?> qp = new QueryParameter<>(position, parameterValueFactory);
        uniqueParams.put(position, qp);
        return qp;
    }

    private void wordEnd() {
        if (SELECT.equalsIgnoreCase(currentWord.toString())) {
            this.inProjection = true;
        } else if (inProjection && WHERE.equalsIgnoreCase(currentWord.toString())) {
            this.inProjection = false;
        }
        currentWord = new StringBuilder();
    }
}
