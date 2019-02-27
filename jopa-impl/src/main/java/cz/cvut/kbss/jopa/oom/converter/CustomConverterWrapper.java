/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.model.AttributeConverter;

import java.util.Objects;

/**
 * Instances of this class are used to wrap user-defined {@link AttributeConverter} implementation.
 */
public class CustomConverterWrapper<X, Y> implements ConverterWrapper<X, Y> {

    private final AttributeConverter<X, Y> wrapped;
    private final Class<Y> axiomValueType;

    public CustomConverterWrapper(AttributeConverter<X, Y> wrapped, Class<Y> axiomValueType) {
        this.wrapped = Objects.requireNonNull(wrapped);
        this.axiomValueType = axiomValueType;
    }

    @Override
    public Y convertToAxiomValue(X value) {
        return wrapped.convertToAxiomValue(value);
    }

    @Override
    public X convertToAttribute(Y value) {
        return wrapped.convertToAttribute(value);
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return axiomValueType.isAssignableFrom(type);
    }
}
