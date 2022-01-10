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
package cz.cvut.kbss.jopa.query;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValue;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;

import java.util.Objects;

public class QueryParameter<T> implements Parameter<T> {

    private final String name;
    private final Integer position;

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

    public ParameterValue getValue() {
        return value;
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
        this.value =
                name != null ? valueFactory.createVariableValue(name) : valueFactory.createVariableValue(position);
    }

    @Override
    public Class<T> getParameterType() {
        throw new IllegalStateException("Parameter types are not supported by the current implementation.");
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
