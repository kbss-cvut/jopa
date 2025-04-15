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
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.QueryAttribute;
import cz.cvut.kbss.jopa.oom.exception.EntityDeconstructionException;
import cz.cvut.kbss.jopa.oom.exception.EntityReconstructionException;
import cz.cvut.kbss.jopa.oom.exception.UnpersistedChangeException;
import cz.cvut.kbss.jopa.sessions.AbstractUnitOfWork;
import cz.cvut.kbss.jopa.sessions.ReadOnlyUnitOfWork;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.sessions.cache.Descriptors;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException.individualAlreadyManaged;

public class ObjectOntologyMapperImpl implements ObjectOntologyMapper, EntityMappingHelper {

    private static final Logger LOG = LoggerFactory.getLogger(ObjectOntologyMapperImpl.class);

    private final AbstractUnitOfWork uow;
    private final Connection storageConnection;

    private final AxiomDescriptorFactory descriptorFactory;
    private final EntityConstructor entityBuilder;
    private final EntityDeconstructor entityBreaker;
    private Map<URI, Object> instanceRegistry;
    private final PendingReferenceRegistry pendingReferences;

    private final EntityInstanceLoader defaultInstanceLoader;
    private final EntityInstanceLoader twoStepInstanceLoader;

    private final EntityReferenceFactory referenceFactory;

    public ObjectOntologyMapperImpl(AbstractUnitOfWork uow, Connection connection) {
        this.uow = Objects.requireNonNull(uow);
        this.storageConnection = Objects.requireNonNull(connection);
        this.descriptorFactory = new AxiomDescriptorFactory();
        this.instanceRegistry = new HashMap<>();
        this.pendingReferences = new PendingReferenceRegistry();
        this.entityBuilder = new EntityConstructor(this, uow.getLoadStateRegistry());
        this.entityBreaker = new EntityDeconstructor(this);

        this.defaultInstanceLoader = DefaultInstanceLoader.builder().connection(storageConnection)
                                                          .metamodel(uow.getMetamodel())
                                                          .descriptorFactory(descriptorFactory)
                                                          .entityBuilder(entityBuilder).cache(getCache())
                                                          .loadStateRegistry(uow.getLoadStateRegistry()).build();
        this.twoStepInstanceLoader = TwoStepInstanceLoader.builder().connection(storageConnection)
                                                          .metamodel(uow.getMetamodel())
                                                          .descriptorFactory(descriptorFactory)
                                                          .entityBuilder(entityBuilder).cache(getCache())
                                                          .loadStateRegistry(uow.getLoadStateRegistry()).build();
        this.referenceFactory = new EntityReferenceFactory(uow.getMetamodel(), uow);
    }

    private CacheManager getCache() {
        return uow.getLiveObjectCache();
    }

    @Override
    public <T> boolean containsEntity(Class<T> cls, URI identifier, Descriptor descriptor) {
        assert cls != null;
        assert identifier != null;
        assert descriptor != null;

        final EntityType<T> et = getEntityType(cls);
        final NamedResource classUri = NamedResource.create(et.getIRI().toURI());
        final Axiom<NamedResource> ax = new AxiomImpl<>(NamedResource.create(identifier),
                Assertion.createClassAssertion(false), new Value<>(classUri));
        try {
            return storageConnection.contains(ax, descriptor.getContexts());
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    @Override
    public <T> T loadEntity(LoadingParameters<T> loadingParameters) {
        assert loadingParameters != null;

        this.instanceRegistry = new HashMap<>();
        return loadEntityInternal(loadingParameters);
    }

    private <T> T loadEntityInternal(LoadingParameters<T> loadingParameters) {
        final IdentifiableEntityType<T> et = getEntityType(loadingParameters.getEntityClass());
        final T result;
        if (et.hasSubtypes()) {
            result = twoStepInstanceLoader.loadEntity(loadingParameters);
        } else {
            result = defaultInstanceLoader.loadEntity(loadingParameters);
        }
        if (result != null) {
            final LoadStateDescriptor<T> loadStateDescriptor = uow.getLoadStateRegistry().get(result);
            assert loadStateDescriptor != null;
            if (!loadingParameters.shouldBypassCache()) {
                getCache().add(loadingParameters.getIdentifier(), result, new Descriptors(loadingParameters.getDescriptor(), loadStateDescriptor));
            }
        }
        return result;
    }

    @Override
    public <T> T getReference(LoadingParameters<T> loadingParameters) {
        assert loadingParameters != null;

        return referenceFactory.createReferenceProxy(loadingParameters);
    }

    @Override
    public <T> IdentifiableEntityType<T> getEntityType(Class<T> cls) {
        return uow.getMetamodel().entity(cls);
    }

    @Override
    public boolean isManagedType(Class<?> cls) {
        return uow.isEntityType(cls);
    }

    @Override
    public <T> void loadFieldValue(T entity, FieldSpecification<? super T, ?> fieldSpec, Descriptor descriptor) {
        assert entity != null;
        assert fieldSpec != null;
        assert descriptor != null;

        LOG.trace("Lazily loading value of field {} of entity {}.", fieldSpec, uow.stringify(entity));

        final EntityType<T> et = (EntityType<T>) getEntityType(entity.getClass());
        final URI identifier = EntityPropertiesUtils.getIdentifier(entity, et);

        if (et.hasQueryAttribute(fieldSpec.getName())) {
            QueryAttribute<? super T, ?> queryAttribute = (QueryAttribute<? super T, ?>) fieldSpec;
            entityBuilder.setQueryAttributeFieldValue(entity, queryAttribute, et);
            return;
        }

        final AxiomDescriptor axiomDescriptor =
                descriptorFactory.createForFieldLoading(identifier, fieldSpec, descriptor, et);
        try {
            final Collection<Axiom<?>> axioms = storageConnection.find(axiomDescriptor);
            entityBuilder.setFieldValue(entity, fieldSpec, axioms, et, descriptor);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        } catch (IllegalArgumentException e) {
            throw new EntityReconstructionException(e);
        }
    }

    @Override
    public <T> void persistEntity(URI identifier, T entity, Descriptor descriptor) {
        assert identifier != null;
        assert entity != null;
        assert descriptor != null;

        @SuppressWarnings("unchecked") final EntityType<T> et = (EntityType<T>) getEntityType(entity.getClass());
        try {
            entityBreaker.setReferenceSavingResolver(new ReferenceSavingResolver(this));
            final AxiomValueGatherer axiomBuilder = entityBreaker.mapEntityToAxioms(identifier, entity, et, descriptor);
            axiomBuilder.persist(storageConnection);
            persistPendingReferences(entity, axiomBuilder.getSubjectIdentifier());
        } catch (IllegalArgumentException e) {
            throw new EntityDeconstructionException("Unable to deconstruct entity " + entity, e);
        }
    }

    @Override
    public URI generateIdentifier(EntityType<?> et) {
        try {
            return storageConnection.generateIdentifier(et.getIRI().toURI());
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    private <T> void persistPendingReferences(T instance, NamedResource identifier) {
        try {
            final Set<PendingAssertion> pas = pendingReferences.removeAndGetPendingAssertionsWith(instance);
            for (PendingAssertion pa : pas) {
                final AxiomValueDescriptor desc = new AxiomValueDescriptor(pa.getOwner());
                desc.addAssertionValue(pa.getAssertion(), new Value<>(identifier));
                desc.setAssertionContext(pa.getAssertion(), pa.getContext());
                storageConnection.persist(desc);
            }
            final Set<PendingReferenceRegistry.PendingListReference> pLists =
                    pendingReferences.removeAndGetPendingListReferencesWith(instance);
            final EntityType<?> et = getEntityType(instance.getClass());
            for (PendingReferenceRegistry.PendingListReference list : pLists) {
                final ListValueDescriptor desc = list.getDescriptor();
                ListPropertyStrategy.addIndividualsToDescriptor(desc, list.getValues(), et);
                if (desc instanceof SimpleListValueDescriptor) {
                    // This can be a persist or an update, calling update works for both
                    storageConnection.lists().updateSimpleList((SimpleListValueDescriptor) desc);
                } else {
                    storageConnection.lists().updateReferencedList((ReferencedListValueDescriptor) desc);
                }
            }
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    @Override
    public <T> T getEntityFromCacheOrOntology(Class<T> cls, URI identifier, Descriptor descriptor) {
        final T orig = uow.getManagedOriginal(cls, identifier, descriptor);
        if (orig != null) {
            return orig;
        }
        if (getCache().contains(cls, identifier, descriptor)) {
            return defaultInstanceLoader.loadCached(getEntityType(cls), identifier, descriptor);
        } else if (instanceRegistry.containsKey(identifier)) {
            final Object existing = instanceRegistry.get(identifier);
            if (!cls.isAssignableFrom(existing.getClass())) {
                throw individualAlreadyManaged(identifier);
            }
            // This prevents endless cycles in bidirectional relationships
            return cls.cast(existing);
        } else {
            // setup loading params
            LoadingParameters<T> params = new LoadingParameters<>(cls, identifier, descriptor);

            // TODO:
            // This is necessary when loading object properties (singular and plural)
            // The solution is not ideal. I think that LoadingParams
            // should be propagated to this method by loading appropriate methods.
            if (uow instanceof ReadOnlyUnitOfWork) {
                // this prevents caching of entities loaded by ReadOnlyUOW
                params.bypassCache();
            }
            return loadEntityInternal(params);
        }
    }

    @Override
    public <T> T getOriginalInstance(T clone) {
        assert clone != null;
        return (T) uow.getOriginal(clone);
    }

    boolean isManaged(Object instance) {
        return uow.isObjectManaged(instance);
    }

    <T> void registerInstance(URI identifier, T instance) {
        instanceRegistry.put(identifier, instance);
    }

    @Override
    public void checkForUnpersistedChanges() {
        if (pendingReferences.hasPendingResources()) {
            throw new UnpersistedChangeException(
                    "The following instances were neither persisted nor marked as cascade for persist: "
                            + pendingReferences.getPendingResources());
        }
    }

    void registerPendingAssertion(NamedResource owner, Assertion assertion, Object object, URI context) {
        pendingReferences.addPendingAssertion(owner, assertion, object, context);
    }

    void registerPendingListReference(Object item, ListValueDescriptor listDescriptor, List<?> values) {
        pendingReferences.addPendingListReference(item, listDescriptor, values);
    }

    @Override
    public <T> void removeEntity(URI identifier, Class<T> cls, Descriptor descriptor) {
        final EntityType<T> et = getEntityType(cls);
        final AxiomDescriptor axiomDescriptor = descriptorFactory.createForEntityLoading(
                new LoadingParameters<>(cls, identifier, descriptor, true), et);
        try {
            storageConnection.remove(axiomDescriptor);
            pendingReferences.removePendingReferences(axiomDescriptor.getSubject());
        } catch (OntoDriverException e) {
            throw new StorageAccessException("Exception caught when removing entity.", e);
        }
    }

    @Override
    public <T> void updateFieldValue(T entity, FieldSpecification<? super T, ?> fieldSpec,
                                     Descriptor entityDescriptor) {
        @SuppressWarnings("unchecked") final EntityType<T> et = (EntityType<T>) getEntityType(entity.getClass());
        final URI pkUri = EntityPropertiesUtils.getIdentifier(entity, et);

        entityBreaker.setReferenceSavingResolver(new ReferenceSavingResolver(this));
        // It is OK to do it like this, because if necessary, the mapping will re-register a pending assertion
        removePendingAssertions(fieldSpec, pkUri);
        final AxiomValueGatherer axiomBuilder =
                entityBreaker.mapFieldToAxioms(pkUri, entity, fieldSpec, et, entityDescriptor);
        axiomBuilder.update(storageConnection);
    }

    private <T> void removePendingAssertions(FieldSpecification<? super T, ?> fs, URI identifier) {
        if (fs instanceof Attribute<?, ?> att) {
            // We care only about object property assertions, others are never pending
            final Assertion assertion = Assertion.createObjectPropertyAssertion(att.getIRI().toURI(), att.isInferred());
            pendingReferences.removePendingReferences(NamedResource.create(identifier), assertion);
        }
    }

    @Override
    public Collection<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor listDescriptor) {
        try {
            return storageConnection.lists().loadSimpleList(listDescriptor);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    @Override
    public Collection<Axiom<?>> loadReferencedList(ReferencedListDescriptor listDescriptor) {
        try {
            return storageConnection.lists().loadReferencedList(listDescriptor);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    public Collection<Axiom<?>> loadRdfContainer(ContainerDescriptor containerDescriptor) {
        try {
            return storageConnection.containers().readContainer(containerDescriptor);
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    @Override
    public Configuration getConfiguration() {
        return uow.getConfiguration();
    }

    @Override
    public <T> Set<Axiom<?>> getAttributeAxioms(T entity, FieldSpecification<? super T, ?> fieldSpec,
                                                Descriptor entityDescriptor) {
        final EntityType<T> et = (EntityType<T>) getEntityType(entity.getClass());
        return FieldStrategy.createFieldStrategy(et, fieldSpec, entityDescriptor, this).buildAxiomsFromInstance(entity);
    }

    @Override
    public boolean isInferred(Axiom<?> axiom, URI context) {
        try {
            return storageConnection.isInferred(axiom, context != null ? Collections.singleton(context) :
                    Collections.emptySet());
        } catch (OntoDriverException e) {
            throw new StorageAccessException(e);
        }
    }

    @Override
    public <T> boolean isInferred(T entity, FieldSpecification<? super T, ?> fieldSpec, Object value,
                                  Descriptor entityDescriptor) {
        final EntityType<T> et = (EntityType<T>) getEntityType(entity.getClass());
        final FieldStrategy<?, ?> fs = FieldStrategy.createFieldStrategy(et, fieldSpec, entityDescriptor, this);
        final Collection<Value<?>> values = fs.toAxiomValue(value);
        final Set<URI> contexts = entityDescriptor.getAttributeContexts(fieldSpec);
        final NamedResource subject = NamedResource.create(EntityPropertiesUtils.getIdentifier(entity, et));
        return values.stream().map(v -> {
            try {
                return storageConnection.isInferred(new AxiomImpl<>(subject, fs.createAssertion(), v), contexts);
            } catch (OntoDriverException e) {
                throw new StorageAccessException(e);
            }
        }).reduce(false, Boolean::logicalOr);
    }

    public UnitOfWork getUow() {
        return uow;
    }
}
