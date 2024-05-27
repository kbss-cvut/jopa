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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadStateDescriptorRegistry;
import cz.cvut.kbss.jopa.utils.Wrapper;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

/**
 * Represents a persistence context.
 * <p>
 * All interactions with objects managed in a persistence context are tracked by its corresponding UoW and on commit,
 * the UoW propagates them into the repository.
 */
public interface UnitOfWork extends ConfigurationHolder, MetamodelProvider, Wrapper {

    /**
     * Clears this Unit of Work.
     */
    void clear();

    /**
     * Notifies this Unit of Work that a transaction has begun.
     */
    void begin();

    /**
     * Commit changes to the repository.
     */
    void commit();

    /**
     * Rolls back changes done since last commit.
     *
     * @see #commit()
     */
    void rollback();

    /**
     * Checks whether the specified entity is managed in this Unit of Work.
     *
     * @param entity Entity to check
     * @return {@literal true} if entity is managed, {@literal false} otherwise
     */
    boolean contains(Object entity);

    /**
     * Is this Unit of Work active?
     *
     * @return boolean
     */
    boolean isActive();

    /**
     * Returns true if this {@code UnitOfWork} represents the persistence context of a currently running transaction.
     *
     * @return True if in an active transaction
     */
    boolean isInTransaction();

    /**
     * Returns true if this Unit of Work is currently committing changes to the repository.
     *
     * @return {@code true} if the UoW is in commit, {@code false} otherwise
     */
    boolean isInCommit();

    /**
     * Return true if the given entity is managed.
     * <p>
     * This means it is tracked by this persistence context either as a new object or an existing object loaded from the
     * repository.
     *
     * @param entity Object to check
     * @return {@code true} when the entity is managed, {@code false} otherwise
     */
    boolean isObjectManaged(Object entity);

    /**
     * Checks whether the specified entity has been registered in this Unit of Work as a new object for persist.
     *
     * @param entity Object to check
     * @return {@code true} when entity is managed and new, {@code false} otherwise
     * @see #isObjectManaged(Object)
     */
    boolean isObjectNew(Object entity);

    /**
     * Checks whether the specified repository context is consistent.
     *
     * @param context Context URI, {@code null} indicates the whole repository should be checked
     * @return {@code true} if the context is consistent, {@code false} otherwise
     * @throws OWLPersistenceException If an ontology access error occurs
     */
    boolean isConsistent(URI context);

    /**
     * Loads value of the specified field for the specified entity.
     * <p>
     * The value is set on the entity.
     *
     * @param entity    The entity to load field for
     * @param fieldSpec Metamodel element representing the field to load
     * @return The loaded field value
     * @throws NullPointerException    If {@code entity} or {@code field} is {@code null}
     * @throws OWLPersistenceException If an error occurs, this may be e.g. that the field is not present on the entity,
     *                                 an ontology access error occurred etc.
     */
    <T> Object loadEntityField(T entity, FieldSpecification<? super T, ?> fieldSpec);

    /**
     * Merges the state of the given entity into the current persistence context.
     * <p>
     * The {@code descriptor} argument specified the ontology contexts into which the detached entity and its fields
     * belong and should be merged.
     *
     * @param entity     entity instance
     * @param descriptor Entity descriptor, specifies repository context
     * @return the managed instance that the state was merged to
     * @throws NullPointerException If {@code entity} or {@code repository} is {@code null}
     */
    <T> T mergeDetached(T entity, Descriptor descriptor);

    /**
     * Retrieves object with the specified identifier.
     * <p>
     * The object as well as its fields are looked for in contexts specified by the descriptor. The result is then cast
     * to the specified type.
     *
     * @param cls        The type of the returned object
     * @param identifier Instance identifier
     * @param descriptor Entity descriptor
     * @return The retrieved object or {@code null} if there is no object with the specified identifier in the specified
     * repository
     * @throws NullPointerException    If {@code cls}, {@code identifier} or {@code repository} is {@code null}
     * @throws OWLPersistenceException If an error occurs during object loading
     */
    <T> T readObject(Class<T> cls, Object identifier, Descriptor descriptor);

    /**
     * Reads an object but does not register it with this persistence context.
     * <p>
     * Useful when the caller knows the object will be registered eventually by another routine.
     *
     * @param cls        Expected result class
     * @param identifier Object identifier
     * @param descriptor Entity descriptor
     * @return The retrieved object or {@code null} if there is no object with the specified identifier in the specified
     * repository
     */
    <T> T readObjectWithoutRegistration(Class<T> cls, Object identifier, Descriptor descriptor);

    /**
     * Retrieves a reference to an object with the specified identifier.
     * <p>
     * A reference is permitted to have its state fetched lazily.
     *
     * @param cls        The type of the returned object
     * @param identifier Instance identifier
     * @param descriptor Entity descriptor
     * @param <T>        Entity type
     * @return The retrieved object or {@code null} if none can be found
     * @throws OWLPersistenceException If an error occurs during object loading
     */
    <T> T getReference(Class<T> cls, Object identifier, Descriptor descriptor);

    /**
     * Register an existing object in this Unit of Work.
     * <p>
     * This is a shortcut for {@link #registerExistingObject(Object, CloneRegistrationDescriptor)}.
     *
     * @param object     Object
     * @param descriptor Entity descriptor identifying repository contexts
     * @return Registered clone of the specified object
     * @see #registerExistingObject(Object, CloneRegistrationDescriptor)
     */
    Object registerExistingObject(Object object, Descriptor descriptor);

    /**
     * Register an existing object in this Unit of Work.
     * <p>
     * Creates a working clone of the specified object according to the configuration and puts the given object into
     * this Unit of Work cache.
     *
     * @param object                 Object
     * @param registrationDescriptor Configuration of the registration
     * @return Registered clone of the specified object
     */
    Object registerExistingObject(Object object, CloneRegistrationDescriptor registrationDescriptor);

    /**
     * Registers the specified new object in this Unit of Work.
     * <p>
     * The object will be persisted into the context specified by {@code descriptor}.
     *
     * @param object     The object to register
     * @param descriptor Entity descriptor
     * @throws NullPointerException    If {@code entity} or {@code context} is {@code null}
     * @throws OWLPersistenceException If {@code context} is not a valid context URI or if an error during registration
     *                                 occurs
     */
    void registerNewObject(Object object, Descriptor descriptor);

    /**
     * Remove the given object from the repository.
     *
     * @param object Object to remove
     */
    void removeObject(Object object);

    /**
     * Restores the specified removed object.
     * <p>
     * This means it is reinstated as a managed entity and reinserted into the repository.
     *
     * @param entity The object to restore
     */
    void restoreRemovedObject(Object entity);

    /**
     * Puts the specified object into the live object cache.
     *
     * @param identifier Object identifier
     * @param entity     Object to cache
     * @param descriptor Descriptor of repository context
     */
    void putObjectIntoCache(Object identifier, Object entity, Descriptor descriptor);

    /**
     * Removes the specified object from the live object cache.
     * <p>
     * This is particularly meant for merging deleted objects from transactions.
     *
     * @param object  Object to remove from cache
     * @param context Entity context URI
     */
    void removeObjectFromCache(Object object, URI context);

    /**
     * Releases this unit of work.
     * <p>
     * Releasing an active Unit of Work with uncommitted changes causes all pending changes to be discarded.
     */
    void release();

    /**
     * Refreshes the state of the specified object from the repository, overwriting any changes made to it.
     *
     * @param object The object to revert
     * @param <T>    Object type
     * @throws IllegalArgumentException If the object is not managed
     */
    <T> void refreshObject(T object);

    /**
     * Finds clone of the specified original object.
     *
     * @param original The original object whose clone we are looking for
     * @return The clone or null, if there is none
     */
    Object getCloneForOriginal(Object original);

    /**
     * Detaches the specified registered object from this Unit of Work.
     *
     * @param object Clone to detach
     */
    void unregisterObject(Object object);

    /**
     * Writes any uncommitted changes into the ontology.
     */
    void writeUncommittedChanges();

    /**
     * Gets repository contexts available to this session.
     *
     * @return Unmodifiable list of context URIs
     */
    List<URI> getContexts();

    /**
     * Gets the registry of entity load state descriptors.
     *
     * @return {@code LoadStateDescriptorRegistry} for this persistence context
     */
    LoadStateDescriptorRegistry getLoadStateRegistry();

    /**
     * Gets the load status of the specified attribute on the specified entity.
     *
     * @param entity        Entity instance
     * @param attributeName Attribute whose load status is to be determined
     * @return Attribute load status
     * @see cz.cvut.kbss.jopa.model.ProviderUtil#isLoadedWithoutReference(Object, String)
     */
    LoadState isLoaded(Object entity, String attributeName);

    /**
     * Gets the load status of the specified entity.
     *
     * @param entity Entity whose load status is to be determined.
     * @return Entity load status
     * @see cz.cvut.kbss.jopa.model.ProviderUtil#isLoaded(Object)
     */
    LoadState isLoaded(Object entity);

    /**
     * Gets the lifecycle state of the specified entity.
     * <p>
     * Note that since no repository is specified we can only determine if the entity is managed or removed. Therefore,
     * if the case is different this method returns {@link EntityState#NOT_MANAGED}.
     *
     * @param entity Entity whose state to resolve
     * @return Entity state
     */
    EntityState getState(Object entity);

    /**
     * Gets the lifecycle state of the specified entity with respect to a repository context indicated by the specified
     * descriptor.
     *
     * @param entity     Entity whose state to resolve
     * @param descriptor Descriptor of repository contexts
     * @return Entity state
     */
    EntityState getState(Object entity, Descriptor descriptor);

    /**
     * Checks whether the specified attribute value of the specified entity is inferred in the underlying repository.
     * <p>
     * Note that given the nature of the repository implementation, this method may return true if the corresponding
     * statement is both inferred and asserted. Also note that this method will use the descriptor associated with the
     * specified entity in this persistence context to resolve the repository context, but some underlying repositories
     * do not store inferences in data contexts, so the attribute context may be ignored.
     *
     * @param entity    Entity whose attribute to examine. Must be managed by this persistence context
     * @param attribute Attribute whose value to examine
     * @param value     The value whose inference to examine
     * @return {@code true} if the entity attribute value is inferred, {@code false} otherwise
     */
    <T>

    boolean isInferred(T entity, FieldSpecification<? super T, ?> attribute, Object value);

    /**
     * Persists changed value of the specified field.
     *
     * @param entity Entity with changes (the clone)
     * @param f      The field whose value has changed
     * @throws IllegalStateException If this UoW is not in transaction
     * @see #attributeChanged(Object, FieldSpecification)
     */
    void attributeChanged(Object entity, Field f);

    /**
     * Persists changed value of the specified field.
     *
     * @param entity    Entity with changes (the clone)
     * @param fieldSpec Metamodel element representing the attribute that changed
     * @throws IllegalStateException If this UoW is not in transaction
     */
    void attributeChanged(Object entity, FieldSpecification<?, ?> fieldSpec);

    /**
     * Creates an indirect collection that wraps the specified collection instance and propagates changes to this
     * persistence context.
     *
     * @param collection Collection to be proxied
     * @param owner      Collection owner instance
     * @param field      Field filled with the collection
     * @return Indirect collection
     */
    Object createIndirectCollection(Object collection, Object owner, Field field);

    /**
     * Gets a {@link SparqlQueryFactory} instance associated with this persistence context.
     *
     * @return SPARQL query factory
     */
    SparqlQueryFactory sparqlQueryFactory();

    /**
     * Gets a {@link CriteriaBuilder} instance for building Criteria API queries.
     *
     * @return Criteria query builder
     */
    CriteriaBuilder getCriteriaBuilder();
}
