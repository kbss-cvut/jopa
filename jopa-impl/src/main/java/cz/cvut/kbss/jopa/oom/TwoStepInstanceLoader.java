/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.oom.metamodel.PolymorphicEntityTypeResolver;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

class TwoStepInstanceLoader extends EntityInstanceLoader {

    private TwoStepInstanceLoader(TwoStepInstanceLoaderBuilder builder) {
        super(builder);
    }

    @Override
    <T> T loadEntity(LoadingParameters<T> loadingParameters) {
        final IdentifiableEntityType<T> rootEt = metamodel.entity(loadingParameters.entityClass());
        try {
            final IdentifiableEntityType<? extends T> et = resolveEntityType(loadingParameters, rootEt);
            if (et == null) {
                return null;
            }
            return loadInstance(loadingParameters, et);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    @Override
    <T> T loadEntityFromAxioms(LoadingParameters<T> loadingParameters, Collection<Axiom<?>> axioms) {
        final IdentifiableEntityType<T> rootEt = metamodel.entity(loadingParameters.entityClass());
        NamedResource individual = NamedResource.create(loadingParameters.identifier());
        final IdentifiableEntityType<? extends T> et = new PolymorphicEntityTypeResolver<>(individual, rootEt, axioms.stream()
                                                                                                                     .filter(ax -> ax.getAssertion()
                                                                                                                                     .isClassAssertion())
                                                                                                                     .map(ax -> (Axiom<URI>) ax)
                                                                                                                     .collect(Collectors.toSet())).determineActualEntityType();
        if (et == null) {
            return null;
        }
        return reconstructEntityFromAxioms(loadingParameters, et, axioms);
    }

    private <T> IdentifiableEntityType<? extends T> resolveEntityType(LoadingParameters<T> loadingParameters,
                                                                      IdentifiableEntityType<T> rootEt) throws OntoDriverException {
        NamedResource individual = NamedResource.create(loadingParameters.identifier());
        final Set<Axiom<URI>> types = storageConnection.types().getTypes(individual,
                loadingParameters.descriptor().getContexts(), false);
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
