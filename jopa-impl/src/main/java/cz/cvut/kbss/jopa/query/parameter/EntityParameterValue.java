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

import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Objects;

class EntityParameterValue extends AbstractParameterValue {

    private final MetamodelProvider metamodelProvider;

    private final Object value;

    EntityParameterValue(Object value, MetamodelProvider metamodelProvider) {
        this.value = Objects.requireNonNull(value);
        this.metamodelProvider = metamodelProvider;
        assert metamodelProvider.isEntityType(value.getClass());
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "<" + EntityPropertiesUtils.getIdentifier(value, metamodelProvider.getMetamodel()) + ">";
    }
}
