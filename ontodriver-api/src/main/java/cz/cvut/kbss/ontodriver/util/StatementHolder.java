/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.util;

import java.util.*;

/**
 * Used for parsing and managing SPARQL statements.
 */
public class StatementHolder {

    private Map<String, String> paramValues;
    private List<String> paramNames;
    private List<String> statementParts;
    private final String statement;


    public StatementHolder(String statement) {
        Objects.requireNonNull(statement);

        this.statement = statement.trim();
    }

    public void analyzeStatement() {
        this.paramNames = new ArrayList<>();
        this.statementParts = new ArrayList<>();
        // In single-quoted string
        boolean inSQString = false;
        // In double-quoted string
        boolean inDQString = false;
        boolean inParam = false;
        int lastParamEndIndex = 0;
        int paramStartIndex = 0;
        for (int i = 0; i < statement.length(); i++) {
            final char c = statement.charAt(i);
            switch (c) {
                case '\'':
                    inSQString = !inSQString;
                    break;
                case '"':
                    inDQString = !inDQString;
                    break;
                case '?':
                    if (!inSQString && !inDQString) {
                        statementParts.add(statement.substring(lastParamEndIndex, i));
                        paramStartIndex = i + 1;
                        inParam = true;
                    }
                    break;
                case '<':
                case '>':
                case ',':
                case '\n':
                case ')':
                case ' ':
                case '.':
                case ';':
                case '{':
                case '}':
                case '[':
                case ']':
                    if (inParam) {
                        lastParamEndIndex = i;
                        final String param = statement.substring(paramStartIndex, i);
                        paramNames.add(param);
                        inParam = false;
                    }
                    break;
                default:
                    break;
            }
        }
        statementParts.add(statement.substring(lastParamEndIndex));
        this.paramValues = new HashMap<>(paramNames.size());

        assert statementParts.size() == paramNames.size() + 1;
    }

    public void setParameter(String parameterName, String value) {
        ensureState();
        if (!paramNames.contains(parameterName)) {
            throw new IllegalArgumentException("Unknown binding name " + parameterName);
        }
        paramValues.put(parameterName, value);
    }

    private void ensureState() {
        if (paramNames == null) {
            throw new IllegalStateException("Statement has to be analyzed before working with parameters.");
        }
    }

    public String assembleStatement() {
        if (isNotParametrized()) {
            return statement;
        }
        final StringBuilder sb = new StringBuilder(statement.length());
        for (int i = 0; i < paramNames.size(); i++) {
            sb.append(statementParts.get(i));
            final String paramValue = paramValues.get(paramNames.get(i));
            if (paramValue == null) {
                sb.append("?").append(paramNames.get(i));
            } else {
                sb.append(paramValue);
            }
        }
        sb.append(statementParts.get(paramNames.size()));
        return sb.toString();
    }

    public String getStatement() {
        return statement;
    }

    private boolean isNotParametrized() {
        return paramNames == null || paramNames.isEmpty();
    }

    public void clearParameters() {
        ensureState();
        for (String param : paramValues.keySet()) {
            paramValues.put(param, null);
        }
    }
}
