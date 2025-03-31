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

import cz.cvut.kbss.jopa.query.sparql.SparqlConstants;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;

import java.util.Collections;
import java.util.List;

public class SoqlAttribute extends SoqlParameter {

    private static final String TRIPLE_END = " . ";

    private String value;

    private boolean isNot = false;

    private FilterableExpression operator;

    private boolean isOrderBy = false;

    private boolean isGroupBy = false;

    private boolean projected;

    public SoqlAttribute(SoqlNode firstNode) {
        super(firstNode);
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public boolean isNot() {
        return isNot;
    }

    public void setNot(boolean not) {
        isNot = not;
    }

    public void setOperator(FilterableExpression operator) {
        this.operator = operator;
    }

    public FilterableExpression getOperator() {
        return operator;
    }

    public boolean isOrderBy() {
        return isOrderBy;
    }

    public void setOrderBy(boolean orderBy) {
        isOrderBy = orderBy;
    }

    public boolean isGroupBy() {
        return isGroupBy;
    }

    public void setGroupBy(boolean groupBy) {
        isGroupBy = groupBy;
    }

    public boolean isProjected() {
        return projected;
    }

    public void setProjected(boolean projected) {
        this.projected = projected;
    }

    public boolean requiresFilter() {
        return (operator != null && operator.requiresFilterExpression()) || getFirstNode().requiresFilterExpression();
    }

    public boolean isObject() {
        return !getFirstNode().hasChild();
    }

    public boolean isInstanceOf() {
        return !getFirstNode().hasChild() && operator == null;
    }

    public List<String> getFilterExpressions() {
        assert requiresFilter();
        String filterParam = getAsParam();
        final String filterValue = SoqlUtils.soqlVariableToSparqlVariable(value);
        if (getFirstNode().requiresFilterExpression()) {
            filterParam = getFirstNode().toFilterExpression(filterParam, filterValue);
        }
        if (operator != null && operator.requiresFilterExpression()) {
            return Collections.singletonList(operator.toFilterExpression(filterParam, filterValue));
        } else {
            return Collections.singletonList(filterParam + " = " + filterValue);
        }
    }

    public String getBasicGraphPattern(String rootVariable) {
        StringBuilder buildTP = new StringBuilder(rootVariable).append(' ');
        if (isInstanceOf()) {
            buildTP.append(SoqlConstants.RDF_TYPE).append(' ')
                   .append(toIri(getFirstNode())).append(TRIPLE_END);
        } else {
            if (isObject()) {
                return "";
            }
            SoqlNode pointer = getFirstNode().getChild();
            StringBuilder buildParam = new StringBuilder("?");
            buildParam.append(getFirstNode().getValue());
            buildParam.append(pointer.getCapitalizedValue());
            String param;
            if (pointer.hasChild() || value == null) {
                param = "?" + pointer.getValue();
            } else {
                if (requiresFilter()) {
                    param = buildParam.toString();
                } else {
                    param = SoqlUtils.soqlVariableToSparqlVariable(value);
                }
            }
            buildTP.append(toIri(pointer)).append(' ').append(param).append(TRIPLE_END);
            while (pointer.hasChild()) {
                SoqlNode newPointer = pointer.getChild();
                if (newPointer.getIri().isEmpty()) {
                    break;
                }
                buildTP.append('?').append(pointer.getValue())
                       .append(' ').append(toIri(newPointer)).append(' ');
                buildParam.append(newPointer.getCapitalizedValue());
                if (newPointer.hasChild()) {
                    buildTP.append('?').append(pointer.getChild().getValue());
                } else {
                    if (requiresFilter()) {
                        buildTP.append(buildParam);
                    } else {
                        buildTP.append(SoqlUtils.soqlVariableToSparqlVariable(value));
                    }
                }
                buildTP.append(TRIPLE_END);
                pointer = newPointer;
            }
        }
        return buildTP.toString();
    }

    private static String toIri(SoqlNode node) {
        final String nodeIri = node.getIri();
        return SparqlConstants.RDF_TYPE_SHORTCUT.equals(nodeIri) ? nodeIri : IdentifierTransformer.stringifyIri(nodeIri);
    }

    @Override
    public String toString() {
        return (value != null ? value + "." : "") + (getFirstNode() != null ? getFirstNode().toString() : " ");
    }
}
