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

import cz.cvut.kbss.jopa.datatype.util.Pair;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.oom.exception.EntityReconstructionException;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.LoadStateDescriptorRegistry;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.net.URI;
import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * Root of the entity loading strategies.
 */
abstract class EntityInstanceLoader {

    final Connection storageConnection;
    final MetamodelImpl metamodel;

    final CacheManager cache;
    private final AxiomDescriptorFactory descriptorFactory;
    final EntityConstructor entityBuilder;

    final LoadStateDescriptorRegistry loadStateRegistry;

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
        this.loadStateRegistry = builder.loadStateRegistry;
    }

    /**
     * Loads entity based on the specified loading parameters.
     *
     * @param loadingParameters Instance loading parameters
     * @return The loaded instance (possibly {@code null})
     */
    abstract <T> T loadEntity(LoadingParameters<T> loadingParameters);

    <U extends T, T> U loadInstance(LoadingParameters<T> loadingParameters, IdentifiableEntityType<U> et) {
        final URI identifier = loadingParameters.getIdentifier();
        final Descriptor descriptor = loadingParameters.getDescriptor();
        if (isCached(loadingParameters, et)) {
            return loadCached(et, identifier, descriptor);
        }
        final AxiomDescriptor axiomDescriptor = descriptorFactory.createForEntityLoading(loadingParameters, et);
        try {
            final Collection<Axiom<?>> axioms = storageConnection.find(axiomDescriptor);
            removeAsserted(axioms, loadingParameters, et);
            return axioms.isEmpty() ? null : entityBuilder.reconstructEntity(
                    new EntityConstructor.EntityConstructionParameters<>(identifier, et, descriptor, loadingParameters.isForceEager()),
                    axioms);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        } catch (cz.cvut.kbss.jopa.exception.InstantiationException e) {
            throw new EntityReconstructionException(e);
        }
    }

    <T> boolean isCached(LoadingParameters<T> loadingParameters, EntityType<? extends T> et) {
        return !loadingParameters.shouldBypassCache() &&
                cache.contains(et.getJavaType(), loadingParameters.getIdentifier(), loadingParameters.getDescriptor());
    }

    <T> T loadCached(EntityType<T> et, URI identifier, Descriptor descriptor) {
        final T cached = cache.get(et.getJavaType(), identifier, descriptor);
        recursivelyProcessCachedEntityReferences(cached, et, new IdentityHashMap<>(), List.of(
                pair -> loadStateRegistry.put(pair.first(), getLoadStatDescriptor(pair.first(), pair.second())),
                pair -> entityBuilder.populateQueryAttributes(pair.first(), (EntityType<Object>) pair.second())
        ));
        return cached;
    }

    private LoadStateDescriptor<?> getLoadStatDescriptor(Object instance, EntityType<?> et) {
        final LoadStateDescriptor<?> cached = cache.getLoadStateDescriptor(instance);
        if (cached != null) {
            return cached;
        }
        return LoadStateDescriptorFactory.createAllUnknown(instance, (EntityType<Object>) et);
    }

    private void recursivelyProcessCachedEntityReferences(Object instance, EntityType<?> et,
                                                          Map<Object, Object> visited,
                                                          List<Consumer<Pair<Object, EntityType<?>>>> handlers) {
        if (visited.containsKey(instance)) {
            return;
        }
        visited.put(instance, null);
        handlers.forEach(h -> h.accept(new Pair<>(instance, et)));
        et.getAttributes().stream().filter(Attribute::isAssociation).forEach(att -> {
            final Class<?> cls = att.isCollection() ? ((PluralAttribute) att).getElementType()
                                                                             .getJavaType() : att.getJavaType();
            if (!metamodel.isEntityType(cls)) {
                return;
            }
            final Object value = EntityPropertiesUtils.getAttributeValue(att, instance);
            if (value != null) {
                // Resolve the value class instead of using the attribute type, as it may be a subclass at runtime
                if (att.isCollection()) {
                    ((Collection<?>) value).forEach(el -> recursivelyProcessCachedEntityReferences(el, metamodel.entity(el.getClass()), visited, handlers));
                } else {
                    recursivelyProcessCachedEntityReferences(value, metamodel.entity(value.getClass()), visited, handlers);
                }
            }
        });
    }

    /**
     * If any of the specified entity type's fields does not include explicit values, remove such axioms from the
     * specified collection of all axioms.
     *
     * @param allAxioms         All axioms representing an entity
     * @param loadingParameters Entity loading parameters
     * @param et                Entity type
     */
    private <T, U extends T> void removeAsserted(Collection<Axiom<?>> allAxioms, LoadingParameters<T> loadingParameters,
                                                 IdentifiableEntityType<U> et) throws OntoDriverException {
        for (FieldSpecification<? super U, ?> fs : et.getFieldSpecifications()) {
            if (fs.includeExplicit()) {
                continue;
            }
            final AxiomDescriptor desc = descriptorFactory.createForAssertedFieldLoading(loadingParameters.getIdentifier(), fs, loadingParameters.getDescriptor(), et);
            final Collection<Axiom<?>> asserted = storageConnection.find(desc);
            removeAxioms(allAxioms, asserted);
        }
    }

    /**
     * Removes axioms that are present in the second collection from the first collection.
     * <p>
     * Only subject, assertion property and value are compared, assertion inference status is ignored.
     *
     * @param allAxioms Axioms from which to remove
     * @param toRemove  Axioms to remove
     */
    static void removeAxioms(Collection<Axiom<?>> allAxioms, Collection<Axiom<?>> toRemove) {
        allAxioms.removeIf(ax -> toRemove.stream().anyMatch(axToRemove ->
                Objects.equals(ax.getSubject(), axToRemove.getSubject())
                        && Objects.equals(axToRemove.getAssertion().getIdentifier(), ax.getAssertion().getIdentifier())
                        && Objects.equals(ax.getValue(), axToRemove.getValue())));
    }

    abstract static class EntityInstanceLoaderBuilder {
        private Connection storageConnection;
        private MetamodelImpl metamodel;
        private CacheManager cache;

        private AxiomDescriptorFactory descriptorFactory;
        private EntityConstructor entityBuilder;

        private LoadStateDescriptorRegistry loadStateRegistry;

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

        EntityInstanceLoaderBuilder loadStateRegistry(LoadStateDescriptorRegistry loadStateRegistry) {
            this.loadStateRegistry = loadStateRegistry;
            return this;
        }

        abstract EntityInstanceLoader build();
    }
}
