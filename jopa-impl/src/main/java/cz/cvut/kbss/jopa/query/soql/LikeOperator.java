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
 * SOQL ({@code NOT}) {@code LIKE} operator.
 */
class LikeOperator implements FilterableExpression {

    private final boolean isNot;

    private LikeOperator(boolean isNot) {
        this.isNot = isNot;
    }

    @Override
    public String toFilterExpression(String parameter, String value) {
        return (isNot ? "!" : "") + "REGEX(" + parameter + ", " + value + ")";
    }

    static LikeOperator like() {
        return new LikeOperator(false);
    }

    static LikeOperator notLike() {
        return new LikeOperator(true);
    }
}
