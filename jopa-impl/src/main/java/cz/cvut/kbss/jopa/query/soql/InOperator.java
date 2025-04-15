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

/**
 * SOQL ({@code NOT}) {@code IN} operator.
 */
class InOperator implements FilterableExpression {

    private final boolean isNot;

    private InOperator(boolean isNot) {
        this.isNot = isNot;
    }

    @Override
    public String toFilterExpression(String parameter, String value) {
        return parameter + (isNot ? " " + SoqlConstants.NOT : "") + " " + SoqlConstants.IN + " (" + value + ')';
    }

    static InOperator in() {
        return new InOperator(false);
    }

    static InOperator notIn() {
        return new InOperator(true);
    }
}
