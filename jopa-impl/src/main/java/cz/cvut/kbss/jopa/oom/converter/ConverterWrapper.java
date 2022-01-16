/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.model.AttributeConverter;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * Internal wrapper of a {@link AttributeConverter} providing addition methods.
 *
 * @param <X> the type of the entity attribute
 * @param <Y> the type of the axiom/triple value
 */
public interface ConverterWrapper<X, Y> extends AttributeConverter<X, Y> {

    /**
     * Checks whether the wrapped converter supports converting the specified axiom value type.
     *
     * @param type Axiom value type
     * @return Whether the type is supported by this wrapper
     */
    boolean supportsAxiomValueType(Class<?> type);
}
