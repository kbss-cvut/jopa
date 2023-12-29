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
package cz.cvut.kbss.jopa.query;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValue;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;

import java.util.Objects;

public class QueryParameter<T> implements Parameter<T> {

    private final String name;
    private final Integer position;
    private boolean projected;

    private final ParameterValueFactory valueFactory;

    private ParameterValue value;

    public QueryParameter(String name, ParameterValueFactory valueFactory) {
        this.name = name;
        this.valueFactory = valueFactory;
        this.position = null;
        resetValue();
    }

    public QueryParameter(Integer position, ParameterValueFactory valueFactory) {
        this.position = position;
        this.valueFactory = valueFactory;
        this.name = null;
        resetValue();
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Integer getPosition() {
        return position;
    }

    public Object getIdentifier() {
        return name != null ? name : position;
    }

    public String getIdentifierAsQueryString() {
        return createVariableValue().getQueryString();
    }

    public ParameterValue getValue() {
        return value;
    }

    public void setProjected(boolean projected) {
        this.projected = projected;
    }

    public boolean isProjected() {
        return projected;
    }

    public void setValue(Object value) {
        assert value != null;
        this.value = valueFactory.create(value);
    }

    public void setValue(String value, String language) {
        assert value != null;
        this.value = valueFactory.create(value, language);
    }

    public void setUntypedValue(Object value) {
        assert value != null;
        this.value = valueFactory.createUntyped(value);
    }

    public void resetValue() {
        this.value = createVariableValue();
    }

    private ParameterValue createVariableValue() {
        return name != null ? valueFactory.createVariableValue(name) : valueFactory.createVariableValue(position);
    }

    @Override
    public Class<T> getParameterType() {
        throw new IllegalStateException("Parameter types are not supported by the current implementation.");
    }

    @Override
    public String toString() {
        return getIdentifierAsQueryString() + (value.isSet() ? " = " + value : "");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || !Parameter.class.isAssignableFrom(o.getClass())) {
            return false;
        }

        Parameter<?> that = (Parameter<?>) o;
        return Objects.equals(name, that.getName()) && Objects.equals(position, that.getPosition());

    }

    @Override
    public int hashCode() {
        return Objects.hash(getName(), getPosition());
    }
}
