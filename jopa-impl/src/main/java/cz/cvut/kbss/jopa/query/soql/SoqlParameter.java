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

class SoqlParameter {

    private SoqlNode firstNode;

    public SoqlParameter(SoqlNode firstNode) {
        this.firstNode = firstNode;
    }

    public String getAsParam() {
        return SoqlUtils.nodeAsQueryVariable(firstNode);
    }

    public void setFirstNode(SoqlNode firstNode) {
        this.firstNode = firstNode;
    }

    public SoqlNode getFirstNode() {
        return firstNode;
    }

    public String getAsValue(String rootVariable) {
        if (!firstNode.hasChild()) {
            return rootVariable;
        }
        StringBuilder buildParam = new StringBuilder("?");
        SoqlNode pointer = firstNode.getChild();
        buildParam.append(pointer.getValue());
        while (pointer.hasChild()) {
            pointer = pointer.getChild();
            buildParam.append(pointer.getCapitalizedValue());
        }
        return buildParam.toString();
    }

    @Override
    public String toString() {
        return firstNode != null ? firstNode.toString() : getClass().getSimpleName();
    }
}
