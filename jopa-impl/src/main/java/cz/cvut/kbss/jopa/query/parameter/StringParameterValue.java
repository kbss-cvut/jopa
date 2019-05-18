/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.parameter;

import java.util.Objects;

public class StringParameterValue extends AbstractParameterValue {

    private final String value;
    private final String language;

    StringParameterValue(String value) {
        this.value = Objects.requireNonNull(value);
        this.language = null;
    }

    StringParameterValue(String value, String language) {
        this.value = Objects.requireNonNull(value);
        this.language = language;
    }

    @Override
    public String getValue() {
        return value;
    }

    public String getLanguage() {
        return language;
    }

    @Override
    public String getQueryString() {
        return "\"" + getEscapedValue() + "\"" + (language != null ? ("@" + language) : "");
    }

    /**
     * Escapes string characters according to SPARQL spec.
     *
     * @return Escaped value
     * @see <a href="https://www.w3.org/TR/sparql11-query/#grammarEscapes" target="_top">
     * https://www.w3.org/TR/sparql11-query/#grammarEscapes</a>
     */
    private String getEscapedValue() {
        final StringBuilder sb = new StringBuilder(value.length());
        char c;
        for (int i = 0; i < value.length(); i++) {
            c = value.charAt(i);
            switch (c) {
                case '\t':
                    sb.append("\\t");
                    break;
                case '\n':
                    sb.append("\\n");
                    break;
                case '\r':
                    sb.append("\\r");
                    break;
                case '\b':
                    sb.append("\\b");
                    break;
                case '\f':
                    sb.append("\\f");
                    break;
                case '\'':
                    sb.append("\\\'");
                    break;
                case '\"':
                    sb.append("\\\"");
                    break;
                case '\\':
                    sb.append("\\\\");
                    break;
                default:
                    sb.append(c);
            }
        }
        return sb.toString();
    }
}
