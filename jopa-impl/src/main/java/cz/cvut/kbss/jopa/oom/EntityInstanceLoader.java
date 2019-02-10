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
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.exceptions.EntityReconstructionException;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.Objects;

/**
 * Root of the entity loading strategies.
 */
abstract class EntityInstanceLoader {

    final Connection storageConnection;
    final MetamodelImpl metamodel;

    final CacheManager cache;
    private final AxiomDescriptorFactory descriptorFactory;
    final EntityConstructor entityBuilder;

    EntityInstanceLoader(EntityInstanceLoaderBuilder builder) {
        assert builder.storageConnection != null;
        assert builder.metamodel != null;
        assert builder.cache != null;
        assert builder.descriptorFactory != null;
        assert builder.entityBuilder != null;

        this.storageConnection = builder.storageConnection;
        this.metamodel = builder.metamodel;
        this.cache = builder.cache;
        this.descriptorFactory = builder.descriptorFactory;
        this.entityBuilder = builder.entityBuilder;
    }

    /**
     * Loads entity based on the specified loading parameters.
     *
     * @param loadingParameters Instance loading parameters
     * @return The loaded instance (possibly {@code null})
     */
    abstract <T> T loadEntity(LoadingParameters<T> loadingParameters);

    /**
     * Loads entity reference.
     * <p>
     * I.e., the object may contain only the identifier, other attributes could be loaded lazily. However, if a corresponding
     * entity is already cached, it can be returned instead.
     *
     * @param loadingParameters Reference loading parameters. Note that cache bypassing and forced eager loading configuration is ignored
     * @param <T>               Entity type
     * @return Loaded entity reference, possibly {@code null}
     */
    abstract <T> T loadReference(LoadingParameters<T> loadingParameters);

    <T> T loadInstance(LoadingParameters<T> loadingParameters, EntityType<? extends T> et) {
        final URI identifier = loadingParameters.getIdentifier();
        final Descriptor descriptor = loadingParameters.getDescriptor();
        if (isCached(loadingParameters, et)) {
            return cache.get(et.getJavaType(), identifier, descriptor);
        }
        final AxiomDescriptor axiomDescriptor = descriptorFactory.createForEntityLoading(loadingParameters, et);
        try {
            final Collection<Axiom<?>> axioms = storageConnection.find(axiomDescriptor);
            return axioms.isEmpty() ? null : entityBuilder.reconstructEntity(identifier, et, descriptor, axioms);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        } catch (InstantiationException | IllegalAccessException e) {
            throw new EntityReconstructionException(e);
        }
    }

    <T> boolean isCached(LoadingParameters<T> loadingParameters, EntityType<? extends T> et) {
        return !loadingParameters.shouldBypassCache() &&
                cache.contains(et.getJavaType(), loadingParameters.getIdentifier(), loadingParameters.getDescriptor());
    }

    <T> T loadReferenceInstance(LoadingParameters<T> loadingParameters, EntityType<? extends T> et) {
        final URI identifier = loadingParameters.getIdentifier();
        if (isCached(loadingParameters, et)) {
            return cache.get(et.getJavaType(), identifier, loadingParameters.getDescriptor());
        }
        final Axiom<NamedResource> typeAxiom = descriptorFactory.createForReferenceLoading(identifier, et);
        try {
            final boolean contains =
                    storageConnection.contains(typeAxiom, loadingParameters.getDescriptor().getContext());
            return contains ? entityBuilder.createEntityInstance(identifier, et) : null;
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        } catch (InstantiationException | IllegalAccessException e) {
            throw new EntityReconstructionException(e);
        }
    }

    abstract static class EntityInstanceLoaderBuilder {
        private Connection storageConnection;
        private MetamodelImpl metamodel;
        private CacheManager cache;

        private AxiomDescriptorFactory descriptorFactory;
        private EntityConstructor entityBuilder;

        EntityInstanceLoaderBuilder connection(Connection connection) {
            this.storageConnection = Objects.requireNonNull(connection);
            return this;
        }

        EntityInstanceLoaderBuilder metamodel(MetamodelImpl metamodel) {
            this.metamodel = metamodel;
            return this;
        }

        EntityInstanceLoaderBuilder descriptorFactory(AxiomDescriptorFactory factory) {
            this.descriptorFactory = factory;
            return this;
        }

        EntityInstanceLoaderBuilder entityBuilder(EntityConstructor builder) {
            this.entityBuilder = builder;
            return this;
        }

        EntityInstanceLoaderBuilder cache(CacheManager cache) {
            this.cache = cache;
            return this;
        }

        abstract EntityInstanceLoader build();
    }
}
