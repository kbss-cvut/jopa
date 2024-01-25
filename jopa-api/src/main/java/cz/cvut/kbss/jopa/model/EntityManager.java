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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.TransactionRequiredException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;

import java.net.URI;
import java.util.List;
import java.util.Map;

public interface EntityManager extends AutoCloseable {

    /**
     * Make an instance managed and persistent.
     * <p>
     * The entity is persisted into the default context.
     *
     * @param entity entity instance
     * @throws OWLEntityExistsException     if the entity already exists. (The EntityExistsException may be thrown when
     *                                      the persist operation is invoked, or the EntityExistsException or another
     *                                      PersistenceException may be thrown at flush or commit time.)
     * @throws IllegalArgumentException     if not an entity
     * @throws NullPointerException         If {@code entity} is {@code null}
     * @throws TransactionRequiredException if invoked on a container-managed entity manager of type
     *                                      PersistenceContextType.TRANSACTION and there is no transaction.
     * @see #persist(Object, Descriptor)
     */
    void persist(final Object entity);

    /**
     * Make an instance managed and persistent.
     * <p>
     * The {@code descriptor} represents repository and context into which the entity and its fields should be
     * persisted.
     *
     * @param entity     entity instance
     * @param descriptor Entity descriptor
     * @throws OWLEntityExistsException     if the entity already exists. (The EntityExistsException may be thrown when
     *                                      the persist operation is invoked, or the EntityExistsException or another
     *                                      PersistenceException may be thrown at flush or commit time.)
     * @throws IllegalArgumentException     if not an entity
     * @throws NullPointerException         If {@code entity} or {@code descriptor} is {@code null}
     * @throws TransactionRequiredException if invoked on a container-managed entity manager of type
     *                                      PersistenceContextType.TRANSACTION and there is no transaction.
     */
    void persist(final Object entity, final Descriptor descriptor);

    /**
     * Merge the state of the given entity into the current persistence context.
     * <p>
     * The entity is merged into the default repository context.
     *
     * @param entity The entity to merge
     * @return the instance that the state was merged to
     * @throws IllegalArgumentException     if instance is not an entity or is a removed entity
     * @throws TransactionRequiredException if invoked on a container-managed entity manager of type
     *                                      PersistenceContextType.TRANSACTION and there is no transaction.
     */
    <T> T merge(final T entity);

    /**
     * Merge the state of the given entity into the current persistence context and into the repository specified by
     * {@code descriptor}.
     *
     * @param entity     The entity to merge
     * @param descriptor Entity descriptor
     * @return the instance that the state was merged to
     * @throws IllegalArgumentException     if instance is not an entity or is a removed entity
     * @throws TransactionRequiredException if invoked on a container-managed entity manager of type
     *                                      PersistenceContextType.TRANSACTION and there is no transaction.
     */
    <T> T merge(final T entity, Descriptor descriptor);

    /**
     * Remove the entity instance.
     *
     * @param entity The instance to remove
     * @throws IllegalArgumentException     if not an entity or if a detached entity
     * @throws TransactionRequiredException if invoked on a container-managed entity manager of type
     *                                      PersistenceContextType.TRANSACTION and there is no transaction.
     */
    void remove(final Object entity);

    /**
     * Find by identifier.
     * <p>
     * Search for an entity of the specified class and identifier. If the entity instance is contained in the
     * persistence context, it is returned from there.
     *
     * @param entityClass Entity class
     * @param identifier  Entity identifier
     * @return the found entity instance or {@code null} if the entity does not exist in the given ontology context
     * @throws IllegalArgumentException if the first argument does not denote an entity type or the second argument is
     *                                  not a valid type for that entity’s identifier
     * @throws NullPointerException     If {@code entityClass}, {@code identifier} is {@code null}
     */
    <T> T find(final Class<T> entityClass, final Object identifier);

    /**
     * Find by identifier.
     * <p>
     * Search for an entity of the specified class and identifier. If the entity instance is contained in the
     * persistence context, it is returned from there.
     * <p>
     * The {@code descriptor} parameter represents repository and context in which the entity should be looked for.
     *
     * @param entityClass Entity class
     * @param identifier  Entity identifier
     * @param descriptor  Entity descriptor
     * @return the found entity instance or {@code null} if the entity does not exist in the given ontology context
     * @throws IllegalArgumentException if the first argument does not denote an entity type or the second argument is
     *                                  not a valid type for that entity’s identifier
     * @throws NullPointerException     If {@code entityClass}, {@code identifier} or {@code contextUri} is {@code
     *                                  null}
     * @see #getContexts()
     */
    <T> T find(final Class<T> entityClass, final Object identifier, final Descriptor descriptor);

    /**
     * Get an instance, whose state may be lazily fetched.
     * <p>
     * If the requested instance does not exist in the database, {@code null} is returned.
     * <p>
     * The application should not expect that the instance state will be available upon detachment, unless it was
     * accessed by the application while the entity manager was open.
     *
     * @param entityClass entity class
     * @param identifier  identifier of the instance
     * @return the found entity instance
     * @throws IllegalArgumentException if the first argument does not denote an entity type or the second argument is
     *                                  not a valid type for that entity’s identifier
     */
    <T> T getReference(final Class<T> entityClass, final Object identifier);

    /**
     * Get an instance, whose state may be lazily fetched.
     * <p>
     * If the requested instance does not exist in the database, {@code null} is returned.
     * <p>
     * The application should not expect that the instance state will be available upon detachment, unless it was
     * accessed by the application while the entity manager was open.
     * <p>
     * The {@code descriptor} parameter represents configuration of the entity loading (e.g., repository context).
     *
     * @param entityClass entity class
     * @param identifier  identifier of the instance
     * @param descriptor  Entity descriptor
     * @return the found entity instance
     * @throws IllegalArgumentException if the first argument does not denote an entity type or the second argument is
     *                                  not a valid type for that entity’s identifier
     */
    <T> T getReference(final Class<T> entityClass, final Object identifier, final Descriptor descriptor);

    /**
     * Synchronize the persistence context to the underlying database.
     *
     * @throws TransactionRequiredException if there is no transaction
     * @throws OWLPersistenceException      if the flush fails
     */
    void flush();

    /**
     * Refresh the state of the instance from the data source, overwriting changes made to the entity, if any.
     *
     * @param entity The entity instance to refresh
     * @throws IllegalArgumentException     if not an entity or entity is not managed
     * @throws TransactionRequiredException if invoked on a container-managed entity manager of type
     *                                      PersistenceContextType.TRANSACTION and there is no transaction.
     */
    void refresh(final Object entity);

    /**
     * Clear the persistence context, causing all managed entities to become detached. Changes made to entities that
     * have not been flushed to the database will not be persisted.
     */
    void clear();

    /**
     * Remove the given entity from the persistence context, causing a managed entity to become detached. Unflushed
     * changes made to the entity if any (including removal of the entity), will not be synchronized to the database.
     * Entities which previously referenced the detached entity will continue to reference it.
     *
     * @param entity The instance to detach
     * @throws IllegalArgumentException if the instance is not an entity
     */
    void detach(Object entity);

    /**
     * Check if the instance belongs to the current persistence context.
     *
     * @param entity The instance to check
     * @return True if the instance is managed, false otherwise
     * @throws IllegalArgumentException if not an entity
     */
    boolean contains(Object entity);

    /**
     * Checks consistency of the specified context.
     * <p>
     * The context URI can be {@code null}, which indicates that consistency of the whole repository should be
     * verified.
     *
     * @param context Context URI, can be {@code null}
     * @return {@code true} if consistent, {@code false} otherwise
     */
    @NonJPA
    boolean isConsistent(URI context);

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
    <T> boolean isInferred(T entity, FieldSpecification<? super T, ?> attribute, Object value);

    /**
     * Get the properties and hints and associated values that are in effect for the entity manager.
     * <p>
     * Changing the contents of the map does not change the configuration in effect.
     *
     * @return Map of properties and hints in effect for entity manager
     */
    Map<String, Object> getProperties();

    /**
     * Set an entity manager property or hint.
     * <p>
     * If a vendor-specific property or hint is not recognized, it is silently ignored.
     *
     * @param propertyName Name of property or hint
     * @param value        Value for property or hint
     * @throws IllegalArgumentException If the second argument is not valid for the implementation
     */
    void setProperty(String propertyName, Object value);

    /**
     * Create an instance of Query for executing a Java Persistence query language statement.
     *
     * @param qlString a Java Persistence query string
     * @return the new query instance
     * @throws IllegalArgumentException if query string is not valid
     */
    @NonJPA
    Query createQuery(String qlString);

    /**
     * Create an instance of TypedQuery for executing a criteria query.
     *
     * @param criteriaQuery criteria query object
     * @return the new query instance
     */
    @NonJPA
    <T> TypedQuery<T> createQuery(CriteriaQuery<T> criteriaQuery);

    /**
     * Creates an instance of query for executing Java persistence query language statement.
     *
     * @param query       query string
     * @param resultClass result type
     * @return the new query instance
     */
    @NonJPA
    <T> TypedQuery<T> createQuery(String query, Class<T> resultClass);


    /**
     * Create an instance of Query for executing a named query (in native SPARQL).
     *
     * @param name the name of a query defined in metadata
     * @return the new query instance
     * @throws IllegalArgumentException if a query has not been defined with the given name
     */
    Query createNamedQuery(String name);

    /**
     * Create an instance of TypedQuery for executing a SPARQL named query.
     * <p>
     * The select list of the query must contain only a single item, which must be assignable to the type specified by
     * the resultClass argument.
     *
     * @param name        the name of a query defined in metadata
     * @param resultClass the type of the query result
     * @return the new query instance
     * @throws IllegalArgumentException if a query has not been defined with the given name or if the query string is
     *                                  found to be invalid or if the query result is found to not be assignable to the
     *                                  specified type
     */
    <T> TypedQuery<T> createNamedQuery(String name, Class<T> resultClass);

    /**
     * Create an instance of Query for executing a native SPARQL(-DL) query in SPARQL syntax.
     *
     * @param sparqlString a native SPARQL query string
     * @return the new query instance
     */
    Query createNativeQuery(String sparqlString);

    /**
     * Create an instance of Query for executing a native SPARQL(-DL) query returning only specific object type.
     *
     * @param sparqlString a native SQL query string
     * @param resultClass  the class of the resulting instance(s)
     * @return the new query instance
     */
    <T> TypedQuery<T> createNativeQuery(String sparqlString, Class<T> resultClass);

    /**
     * Create an instance of Query for executing a native SPARQL query.
     *
     * @param sparqlString     a native SQL query string
     * @param resultSetMapping the name of the result set mapping
     * @return the new query instance
     */
    Query createNativeQuery(String sparqlString, String resultSetMapping);

    /**
     * Return an object of the specified type to allow access to the provider-specific API. If the provider's
     * EntityManager implementation does not support the specified class, the {@link OWLPersistenceException} is
     * thrown.
     *
     * @param cls The class of the object to be returned. This can be also an implementation of the underlying driver
     * @return an instance of the specified class
     * @throws OWLPersistenceException If the provider does not support the specified class
     */
    <T> T unwrap(Class<T> cls);

    /**
     * Return the underlying provider object for the EntityManager, if available. The result of this method is
     * implementation specific.
     *
     * @return underlying provider object for EntityManager
     */
    Object getDelegate();

    /**
     * Close an application-managed EntityManager. After the close method has been invoked, all methods on the
     * EntityManager instance and any Query objects obtained from it will throw the IllegalStateException except for
     * getTransaction and isOpen (which will return false). If this method is called when the EntityManager is
     * associated with an active transaction, the persistence context remains managed until the transaction completes.
     *
     * @throws IllegalStateException if the EntityManager is container-managed.
     */
    void close();

    /**
     * Determine whether the EntityManager is open.
     *
     * @return true until the EntityManager has been closed.
     */
    boolean isOpen();

    /**
     * Return the resource-level transaction object. The EntityTransaction instance may be used serially to begin and
     * commit multiple transactions.
     *
     * @return EntityTransaction instance
     * @throws IllegalStateException if invoked on a JTA EntityManager.
     */
    EntityTransaction getTransaction();

    /**
     * Return the entity manager factory for the entity manager.
     *
     * @return EntityManagerFactory instance
     * @since JPA 2.0
     */
    EntityManagerFactory getEntityManagerFactory();

    /**
     * Returns a list of repository contexts available to this entity manager.
     *
     * @return List of repository context URIs
     */
    @NonJPA
    List<URI> getContexts();

    /**
     * Return a criteriaFactory for making CriteriaQuery.
     *
     * @return CriteriaBuilder instance
     */
    CriteriaBuilder getCriteriaBuilder();

    /**
     * Return an instance of Metamodel interface for access to the metamodel of the persistence unit.
     *
     * @return Metamodel instance
     */
    Metamodel getMetamodel();
}
