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
package cz.cvut.kbss.jopa.query.parameter;

/**
 * Represents a variable, i.e. an unset parameter value.
 * <p>
 * All query parameters start out as variable values, until real values are set for them.
 */
class PositionalVariableParameterValue extends AbstractParameterValue {

    private final Integer position;

    PositionalVariableParameterValue(Integer position) {
        this.position = position;
    }

    @Override
    public Object getValue() {
        return null;
    }

    @Override
    public String getQueryString() {
        return "$" + position;
    }

    @Override
    public boolean isSet() {
        return false;
    }
}
