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

import java.util.stream.Collectors;

class FunctionNode extends SoqlNode {

    // SOQL function name
    private final String functionName;

    FunctionNode(String functionName, SoqlNode... args) {
        super(args);
        assert args != null && args.length > 0;
        assert functionName != null && !functionName.isEmpty();
        this.functionName = functionName;
    }

    @Override
    public boolean hasChild() {
        return children.stream().anyMatch(SoqlNode::hasChild);
    }

    @Override
    public SoqlNode getChild() {
        return children.get(0).getChild();
    }

    @Override
    public String getValue() {
        return children.get(0).getValue();
    }

    @Override
    public void setValue(String value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getCapitalizedValue() {
        return children.get(0).getCapitalizedValue();
    }

    @Override
    public String getIri() {
        return children.get(0).getIri();
    }

    @Override
    public void setIri(String iri) {
        throw new UnsupportedOperationException();
    }

    @Override
    public SoqlAttribute getSoqlAttribute() {
        assert !children.isEmpty();
        return children.get(0).getSoqlAttribute();
    }

    @Override
    public String toFilterExpression(String filterParam, String filterValue) {
        return SoqlFunctionTranslator.getSparqlFunction(functionName) + "(" +
                children.stream().map(n -> n.toFilterExpression(filterParam, filterValue))
                        .collect(Collectors.joining(", "))
                + ")";
    }

    @Override
    public String toString() {
        return functionName + "(" + children.stream().map(SoqlNode::toString).collect(Collectors.joining(", ")) + ")";
    }
}
