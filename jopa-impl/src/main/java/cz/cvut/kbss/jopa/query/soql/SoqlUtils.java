/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.query.soql;

/**
 * SOQL parsing and processing utilities.
 */
class SoqlUtils {

    private SoqlUtils() {
        throw new AssertionError();
    }

    /**
     * Transforms the specified SOQL variable to a SPARQL variable.
     * <p>
     * This basically means replacing the colon or question mark prefix with a question mark.
     *
     * @param variable SOQL variable name
     * @return SPARQL variable name
     */
    static String soqlVariableToSparqlVariable(String variable) {
        assert variable.length() >= 2 && (variable.charAt(0) == ':' || variable.charAt(0) == '?');
        return "?" + variable.substring(1);
    }

    /**
     * Capitalizes the specified value, transforming the first letter of the specified value to upper case.
     * <p>
     * For example, {@literal value} is transformed to {@literal Value}.
     *
     * @param value Value to capitalize
     * @return capitalized value
     */
    static String capitalize(String value) {
        return value.substring(0, 1).toUpperCase() + value.substring(1);
    }

    /**
     * Creates a SPARQL variable from the specified node.
     * <p>
     * This is done by concatenating capitalized values of the node and its first descendants, the node's value itself
     * is not capitalized, but is prefixed with {@literal ?} demarcating a SPARQL variable.
     *
     * @param node Node to create variable from
     * @return SPARQL variable name
     */
    static String nodeAsQueryVariable(SoqlNode node) {
        StringBuilder buildParam = new StringBuilder("?");
        buildParam.append(node.getValue());
        SoqlNode pointer = node;
        while (pointer.hasChild()) {
            pointer = pointer.getChild();
            buildParam.append(pointer.getCapitalizedValue());
        }
        return buildParam.toString();
    }
}
