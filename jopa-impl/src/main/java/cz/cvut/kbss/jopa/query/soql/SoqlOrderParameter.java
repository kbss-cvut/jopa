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

class SoqlOrderParameter extends SoqlParameter {

    private String orderingBy;

    private SoqlAttribute attribute;

    public SoqlOrderParameter(SoqlNode firstNode, String orderingBy) {
        super(firstNode);
        this.orderingBy = orderingBy.isEmpty() ? SoqlConstants.ASC : orderingBy;
    }

    public String getOrderingBy() {
        return orderingBy;
    }

    public void setOrderingBy(String orderingBy) {
        this.orderingBy = orderingBy;
    }

    public SoqlAttribute getAttribute() {
        return attribute;
    }

    public void setAttribute(SoqlAttribute attribute) {
        this.attribute = attribute;
    }

    public String getOrderByPart(String rootVariable) {
        String param = attribute.requiresFilter() ? getAsParam(rootVariable).substring(1) : attribute.getValue().substring(1);
        return orderingBy + "(?" + param + ") ";
    }
}
