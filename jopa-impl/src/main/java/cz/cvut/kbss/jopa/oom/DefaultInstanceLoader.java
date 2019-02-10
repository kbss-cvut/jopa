/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;

/**
 * Loads entities which do not require polymorphic handling.
 */
class DefaultInstanceLoader extends EntityInstanceLoader {

    private DefaultInstanceLoader(DefaultInstanceLoaderBuilder builder) {
        super(builder);
    }

    @Override
    <T> T loadEntity(LoadingParameters<T> loadingParameters) {
        final EntityType<T> et = metamodel.entity(loadingParameters.getEntityType());
        return loadInstance(loadingParameters, et);
    }

    @Override
    <T> T loadReference(LoadingParameters<T> loadingParameters) {
        final EntityType<T> et = metamodel.entity(loadingParameters.getEntityType());
        return loadReferenceInstance(loadingParameters, et);
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
