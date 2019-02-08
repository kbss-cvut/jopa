/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.EntityTypeImpl;
import cz.cvut.kbss.jopa.oom.metamodel.PolymorphicEntityTypeResolver;
import cz.cvut.kbss.jopa.sessions.FindResult;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Set;

class TwoStepInstanceLoader extends EntityInstanceLoader {

    private TwoStepInstanceLoader(TwoStepInstanceLoaderBuilder builder) {
        super(builder);
    }

    @Override
    <T> FindResult<? extends T> loadEntity(LoadingParameters<T> loadingParameters) {
        final NamedResource individual = NamedResource.create(loadingParameters.getIdentifier());
        final EntityTypeImpl<T> rootEt = metamodel.entity(loadingParameters.getEntityType());
        try {
            final Set<Axiom<URI>> types = storageConnection.types().getTypes(individual,
                    loadingParameters.getDescriptor().getContext(), false);
            final EntityType<? extends T> et =
                    new PolymorphicEntityTypeResolver<>(individual, rootEt, types).determineActualEntityType();
            if (et == null) {
                return FindResult.empty();
            }
            return loadInstance(loadingParameters, et);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    static TwoStepInstanceLoaderBuilder builder() {
        return new TwoStepInstanceLoaderBuilder();
    }

    static class TwoStepInstanceLoaderBuilder extends EntityInstanceLoaderBuilder {

        @Override
        EntityInstanceLoader build() {
            return new TwoStepInstanceLoader(this);
        }
    }
}
