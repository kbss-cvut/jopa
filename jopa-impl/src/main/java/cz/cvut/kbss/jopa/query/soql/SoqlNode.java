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

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.util.ArrayList;
import java.util.List;

abstract class SoqlNode implements FilterableExpression {

    SoqlNode parent;
    SoqlAttribute soqlAttribute;
    final List<SoqlNode> children = new ArrayList<>();

    private FieldSpecification<?, ?> attribute;

    private boolean occursInFilter;

    SoqlNode() {
    }

    SoqlNode(SoqlNode parent) {
        this.parent = parent;
    }

    SoqlNode(SoqlNode... children) {
        this.children.addAll(List.of(children));
    }

    public boolean hasChild() {
        return !children.isEmpty();
    }

    public SoqlNode getChild() {
        return children.get(0);
    }

    public List<SoqlNode> getChildren() {
        return children;
    }

    public SoqlNode getParent() {
        return parent;
    }

    public void addChild(SoqlNode child) {
        children.add(child);
    }

    public void clearChildren() {
        children.clear();
    }

    public void setParent(SoqlNode parent) {
        this.parent = parent;
    }

    public FieldSpecification<?, ?> getAttribute() {
        return attribute;
    }

    public void setAttribute(FieldSpecification<?, ?> attribute) {
        this.attribute = attribute;
    }

    public SoqlAttribute getSoqlAttribute() {
        return soqlAttribute;
    }

    public void setSoqlAttribute(SoqlAttribute soqlAttribute) {
        this.soqlAttribute = soqlAttribute;
    }

    public abstract String getValue();

    public abstract void setValue(String value);

    public abstract String getCapitalizedValue();

    public abstract String getIri();

    public abstract void setIri(String iri);

    public boolean occursInFilter() {
        return occursInFilter;
    }

    public void setOccursInFilter(boolean occursInFilter) {
        this.occursInFilter = occursInFilter;
        children.forEach(n -> n.setOccursInFilter(occursInFilter));
    }
}
