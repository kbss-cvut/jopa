/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.parameter;

/**
 * Represents a variable, i.e. an unset parameter value.
 * <p>
 * All query parameters start out as variable values, until real values are set for them.
 */
class NamedVariableParameterValue extends AbstractParameterValue {

    private final String variableName;

    NamedVariableParameterValue(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public String getValue() {
        return null;
    }

    @Override
    public String getQueryString() {
        return "?" + variableName;
    }

    @Override
    public boolean isSet() {
        return false;
    }
}
