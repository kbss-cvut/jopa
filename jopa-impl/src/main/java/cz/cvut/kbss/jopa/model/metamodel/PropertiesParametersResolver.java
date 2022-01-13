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
package cz.cvut.kbss.jopa.model.metamodel;

import java.lang.reflect.*;

class PropertiesParametersResolver {

    private final java.lang.reflect.Type keyType;
    private final java.lang.reflect.Type valueType;

    PropertiesParametersResolver(Field propertiesField) {
        final ParameterizedType typeSpec = (ParameterizedType) propertiesField.getGenericType();
        assert typeSpec.getActualTypeArguments().length == 2;
        this.keyType = typeSpec.getActualTypeArguments()[0];
        this.valueType = typeSpec.getActualTypeArguments()[1];
    }

    java.lang.reflect.Type getKeyType() {
        return keyType;
    }

    java.lang.reflect.Type getValueType() {
        return valueType;
    }

    Class<?> getPropertyIdentifierType() {
        assert keyType instanceof Class;
        return (Class<?>) keyType;
    }

    Class<?> getPropertyValueType() {
        assert valueType instanceof ParameterizedType;
        final ParameterizedType type = (ParameterizedType) valueType;
        assert type.getActualTypeArguments().length == 1;
        assert type.getActualTypeArguments()[0] instanceof Class;
        return (Class<?>) type.getActualTypeArguments()[0];
    }
}
