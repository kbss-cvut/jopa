/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.soql;

public class SoqlOrderParameter extends SoqlParameter {

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

    public String getOrderByPart() {
        String param = attribute.requiresFilter() ? getAsParam().substring(1) : attribute.getValue().substring(1);
        StringBuilder sb = new StringBuilder();
        if (SoqlConstants.ASC.equals(orderingBy)) {
            sb.append("?").append(param).append(' ');
        } else {
            sb.append(SoqlConstants.DESC).append("(?").append(param).append(") ");
        }
        return sb.toString();
    }
}
