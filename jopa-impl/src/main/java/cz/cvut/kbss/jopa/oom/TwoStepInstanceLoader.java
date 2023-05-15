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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exception.InstantiationException;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.oom.exceptions.EntityReconstructionException;
import cz.cvut.kbss.jopa.oom.metamodel.PolymorphicEntityTypeResolver;
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
    <T> T loadEntity(LoadingParameters<T> loadingParameters) {
        final IdentifiableEntityType<T> rootEt = metamodel.entity(loadingParameters.getEntityType());
        try {
            final EntityType<? extends T> et = resolveEntityType(loadingParameters, rootEt);
            if (et == null) {
                return null;
            }
            return loadInstance(loadingParameters, et);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    @Override
    <T> T loadReference(LoadingParameters<T> loadingParameters) {
        final IdentifiableEntityType<T> rootEt = metamodel.entity(loadingParameters.getEntityType());
        try {
            final EntityType<? extends T> et = resolveEntityType(loadingParameters, rootEt);
            return et != null ? entityBuilder.createEntityInstance(loadingParameters.getIdentifier(), et) : null;
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        } catch (InstantiationException e) {
            throw new EntityReconstructionException(e);
        }
    }

    private <T> EntityType<? extends T> resolveEntityType(LoadingParameters<T> loadingParameters,
                                                          IdentifiableEntityType<T> rootEt) throws OntoDriverException {
        NamedResource individual = NamedResource.create(loadingParameters.getIdentifier());
        final Set<Axiom<URI>> types = storageConnection.types().getTypes(individual,
                loadingParameters.getDescriptor().getContexts(), false);
        return new PolymorphicEntityTypeResolver<>(individual, rootEt, types).determineActualEntityType();
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
