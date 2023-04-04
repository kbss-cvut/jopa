/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.utils.IdentifierTransformer;

public class SoqlAttribute extends SoqlParameter {

    private static final String TRIPLE_END = " . ";

    private String value;

    private boolean isNot = false;

    private FilterOperator operator;

    private boolean isOrderBy = false;

    private boolean isGroupBy = false;

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

    public void setOperator(FilterOperator operator) {
        this.operator = operator;
    }

    public FilterOperator getOperator() {
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

    public boolean isFilter() {
        return operator != null && operator.requiresFilterExpression();
    }

    public boolean isObject() {
        return !getFirstNode().hasNextChild();
    }

    public String getFilter() {
        return operator.toFilterExpression(getAsParam(), toVariable(value));
    }

    public String getTriplePattern(String rootVariable) {
        StringBuilder buildTP = new StringBuilder(rootVariable).append(' ');
        if (isObject()) {
            buildTP.append(SoqlConstants.RDF_TYPE).append(' ')
                   .append(toIri(getFirstNode())).append(TRIPLE_END);
        } else {
            SoqlNode pointer = getFirstNode().getChild();
            StringBuilder buildParam = new StringBuilder("?");
            buildParam.append(getFirstNode().getValue());
            buildParam.append(pointer.getCapitalizedValue());
            String param;
            if (pointer.hasNextChild()) {
                param = "?" + pointer.getValue();
            } else {
                if (isFilter()) {
                    param = buildParam.toString();
                } else {
                    param = toVariable(value);
                }
            }
            buildTP.append(toIri(pointer)).append(' ').append(param).append(TRIPLE_END);
            while (pointer.hasNextChild()) {
                SoqlNode newPointer = pointer.getChild();
                buildTP.append('?').append(pointer.getValue())
                       .append(' ').append(toIri(newPointer)).append(' ');
                buildParam.append(newPointer.getCapitalizedValue());
                if (newPointer.hasNextChild()) {
                    buildTP.append('?').append(pointer.getChild().getValue());
                } else {
                    if (isFilter()) {
                        buildTP.append(buildParam);
                    } else {
                        buildTP.append(toVariable(value));
                    }
                }
                buildTP.append(TRIPLE_END);
                pointer = newPointer;
            }
        }
        return buildTP.toString();
    }

    private static String toIri(SoqlNode node) {
        return IdentifierTransformer.stringifyIri(node.getIri());
    }

    private static String toVariable(String name) {
        return "?" + name.substring(1);
    }
}
