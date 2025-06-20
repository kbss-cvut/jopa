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

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

abstract class SoqlNode implements FilterableExpression {

    SoqlNode parent;
    SoqlNode child;

    private Attribute<?, ?> attribute;

    SoqlNode() {
    }

    SoqlNode(SoqlNode parent) {
        this.parent = parent;
    }

    public boolean hasChild() {
        return child != null;
    }

    public SoqlNode getChild() {
        return child;
    }

    public SoqlNode getParent() {
        return parent;
    }

    public void setChild(SoqlNode child) {
        this.child = child;
    }

    public void setParent(SoqlNode parent) {
        this.parent = parent;
    }

    public Attribute<?, ?> getAttribute() {
        return attribute;
    }

    public void setAttribute(Attribute<?, ?> attribute) {
        this.attribute = attribute;
    }

    public abstract String getValue();

    public abstract void setValue(String value);

    public abstract String getCapitalizedValue();

    public abstract String getIri();

    public abstract void setIri(String iri);
}
