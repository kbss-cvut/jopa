/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
 * Represents a literal used as an argument of a SOQL function.
 * <p>
 * Unlike {@link AttributeNode}, it is emitted into the generated SPARQL filter verbatim (i.e., not as a variable).
 */
class LiteralNode extends SoqlNode {

    private final String value;

    LiteralNode(String value) {
        this.value = value;
    }

    @Override
    public String getValue() {
        return value;
    }

    @Override
    public void setValue(String value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getCapitalizedValue() {
        return SoqlUtils.capitalize(value);
    }

    @Override
    public String getIri() {
        return "";
    }

    @Override
    public void setIri(String iri) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean requiresFilterExpression() {
        return false;
    }

    @Override
    public String toFilterExpression(String filterParam, String filterValue) {
        return value;
    }

    @Override
    public String toString() {
        return value;
    }
}
