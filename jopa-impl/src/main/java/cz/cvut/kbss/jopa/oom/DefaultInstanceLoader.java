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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;

/**
 * Loads entities which do not require polymorphic handling.
 */
class DefaultInstanceLoader extends EntityInstanceLoader {

    private DefaultInstanceLoader(DefaultInstanceLoaderBuilder builder) {
        super(builder);
    }

    @Override
    <T> T loadEntity(LoadingParameters<T> loadingParameters) {
        final IdentifiableEntityType<T> et = metamodel.entity(loadingParameters.getEntityClass());
        return loadInstance(loadingParameters, et);
    }

    static DefaultInstanceLoaderBuilder builder() {
        return new DefaultInstanceLoaderBuilder();
    }

    static class DefaultInstanceLoaderBuilder extends EntityInstanceLoaderBuilder {

        @Override
        EntityInstanceLoader build() {
            return new DefaultInstanceLoader(this);
        }
    }
}
