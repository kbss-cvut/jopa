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

/**
 * SOQL ({@code NOT}) {@code MEMBER OF} operator.
 */
public class MemberOfOperator implements FilterableExpression {

    @Override
    public String toFilterExpression(String parameter, String value) {
        return "";
    }

    @Override
    public boolean requiresFilterExpression() {
        return false;
    }

    static MemberOfOperator memberOf() {
        return new MemberOfOperator();
    }
}
