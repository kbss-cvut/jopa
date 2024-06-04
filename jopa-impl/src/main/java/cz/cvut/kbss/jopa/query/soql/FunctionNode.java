/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

class FunctionNode extends SoqlNode {

    // SOQL function name
    private final String functionName;

    FunctionNode(SoqlNode child, String functionName) {
        assert child != null;
        assert functionName != null && !functionName.isEmpty();
        this.child = child;
        this.functionName = functionName;
    }

    @Override
    public boolean hasChild() {
        return child.hasChild();
    }

    @Override
    public SoqlNode getChild() {
        return child.getChild();
    }

    @Override
    public void setChild(SoqlNode child) {
        child.setChild(child);
    }

    @Override
    public String getValue() {
        return child.getValue();
    }

    @Override
    public void setValue(String value) {
        child.setValue(value);
    }

    @Override
    public String getCapitalizedValue() {
        return child.getCapitalizedValue();
    }

    @Override
    public String getIri() {
        return child.getIri();
    }

    @Override
    public void setIri(String iri) {
        child.setIri(iri);
    }

    @Override
    public boolean requiresFilterExpression() {
        return true;
    }

    @Override
    public String toFilterExpression(String filterParam, String filterValue) {
        return SoqlFunctionTranslator.getSparqlFunction(functionName) + "(" + child.toFilterExpression(filterParam,
                                                                                                       filterValue) + ")";
    }

    @Override
    public String toString() {
        return functionName + "(" + (child != null ? child.toString() : "") + ")";
    }
}
