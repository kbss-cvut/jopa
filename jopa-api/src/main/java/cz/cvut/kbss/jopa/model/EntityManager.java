/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.TransactionRequiredException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;

import java.net.URI;
import java.util.List;

public interface EntityManager {

	/**
	 * Make an instance managed and persistent. </p>
	 * 
	 * The entity is persisted into the default context.
	 * 
	 * @param entity
	 *            entity instance
	 * @throws OWLEntityExistsException
	 *             if the entity already exists. (The EntityExistsException may
	 *             be thrown when the persist operation is invoked, or the
	 *             EntityExistsException or another PersistenceException may be
	 *             thrown at flush or commit time.)
	 * @throws IllegalArgumentException
	 *             if not an entity
	 * @throws NullPointerException
	 *             If {@code entity} is {@code null}
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 * @see {@link #persist(Object, cz.cvut.kbss.jopa.model.descriptors.Descriptor)}
	 */
	public void persist(final Object entity);

	/**
	 * Make an instance managed and persistent. </p>
	 * 
	 * The {@code descriptor} represents repository and context into which the
	 * entity and its fields should be persisted.
	 * 
	 * @param entity
	 *            entity instance
	 * @param descriptor
	 *            Entity descriptor
	 * @throws OWLEntityExistsException
	 *             if the entity already exists. (The EntityExistsException may
	 *             be thrown when the persist operation is invoked, or the
	 *             EntityExistsException or another PersistenceException may be
	 *             thrown at flush or commit time.)
	 * @throws IllegalArgumentException
	 *             if not an entity
	 * @throws NullPointerException
	 *             If {@code entity} or {@code descriptor} is {@code null}
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 */
	public void persist(final Object entity, final Descriptor descriptor);

	/**
	 * Merge the state of the given entity into the current persistence context.
	 * </p>
	 * 
	 * The entity is merged into the default repository context.
	 * 
	 * @param entity
	 *            The entity to merge
	 * @return the instance that the state was merged to
	 * @throws IllegalArgumentException
	 *             if instance is not an entity or is a removed entity
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 */
	public <T> T merge(final T entity);

	/**
	 * Merge the state of the given entity into the current persistence context
	 * and into the repository specified by {@code descriptor}.
	 * 
	 * @param entity
	 *            The entity to merge
	 * @param descriptor
	 *            Entity descriptor
	 * @return the instance that the state was merged to
	 * @throws IllegalArgumentException
	 *             if instance is not an entity or is a removed entity
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 */
	public <T> T merge(final T entity, Descriptor descriptor);

	/**
	 * Remove the entity instance.
	 * 
	 * @param entity The instance to remove
	 * @throws IllegalArgumentException
	 *             if not an entity or if a detached entity
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 */
	public void remove(final Object entity);

	/**
	 * Find by primary key. </p>
	 * 
	 * Search for an entity of the specified class and primary key. If the
	 * entity instance is contained in the persistence context, it is returned
	 * from there. </p>
	 * 
	 * @param entityClass
	 *            Entity class
	 * @param primaryKey
	 *            Primary key
	 * @return the found entity instance or {@code null} if the entity does not
	 *         exist in the given ontology context
	 * @throws IllegalArgumentException
	 *             if the first argument does not denote an entity type or the
	 *             second argument is not a valid type for that entity’s primary
	 *             key
	 * @throws NullPointerException
	 *             If {@code entityClass}, {@code primaryKey} is {@code null}
	 */
	public <T> T find(final Class<T> entityClass, final Object primaryKey);

	/**
	 * Find by primary key. </p>
	 * 
	 * Search for an entity of the specified class and primary key. If the
	 * entity instance is contained in the persistence context, it is returned
	 * from there. </p>
	 * 
	 * The {@code repository} parameter represents repository and context in
	 * which the entity should be looked for.
	 * 
	 * @param entityClass
	 *            Entity class
	 * @param primaryKey
	 *            Primary key
	 * @param descriptor
	 *            Entity descriptor
	 * @return the found entity instance or {@code null} if the entity does not
	 *         exist in the given ontology context
	 * @throws IllegalArgumentException
	 *             if the first argument does not denote an entity type or the
	 *             second argument is not a valid type for that entity’s primary
	 *             key
	 * @throws NullPointerException
	 *             If {@code entityClass}, {@code primaryKey} or
	 *             {@code contextUri} is {@code null}
	 * @see #getContexts()
	 */
	public <T> T find(final Class<T> entityClass, final Object primaryKey,
			final Descriptor descriptor);

	// TODO JPA 2.0 find with properties

	// TODO JPA 2.0 find with lock mode

	// TODO JPA 2.0 find with lock mode and properties

	// /**
	// * Get an instance, whose state may be lazily fetched. If the requested
	// * instance does not exist in the database, the EntityNotFoundException is
	// * thrown when the instance state is first accessed. (The persistence
	// * provider runtime is permitted to throw the EntityNotFoundException when
	// * getReference is called.) The application should not expect that the
	// * instance state will be available upon detachment, unless it was
	// accessed
	// * by the application while the entity manager was open.
	// *
	// * @param entityClass
	// * @param primaryKey
	// * @return the found entity instance
	// * @throws IllegalArgumentException
	// * if the first argument does not denote an entity type or the
	// * second argument is not a valid type for that entity’s primary
	// * key
	// * @throws EntityNotFoundException
	// * if the entity state cannot be accessed
	// */
	// public <T> T getReference(final Class<T> entityClass,
	// final Object primaryKey);

	/**
	 * Synchronize the persistence context to the underlying database.
	 * 
	 * @throws TransactionRequiredException
	 *             if there is no transaction
	 * @throws OWLPersistenceException
	 *             if the flush fails
	 */
	public void flush();

	// /**
	// * Set the flush mode that applies to all objects contained in the
	// * persistence context.
	// *
	// * @param flushMode
	// */
	// public void setFlushMode(FlushModeType flushMode);

	// TODO JPA 2.0 getFlushMode

	// /**
	// * Set the lock mode for an entity object contained in the persistence
	// * context.
	// *
	// * @param entity
	// * @param lockMode
	// * @throws PersistenceException
	// * if an unsupported lock call is made
	// * @throws IllegalArgumentException
	// * if the instance is not an entity or is a detached entity
	// * @throws TransactionRequiredException
	// * if there is no transaction
	// */
	// public void lock(Object entity, LockModeType lockMode);

	// TODO JPA 2.0 lock with lock mode and properties

	/**
	 * Refresh the state of the instance from the data source, overwriting
	 * changes made to the entity, if any. </p>
	 * 
	 * @param entity The entity instance to refresh
	 * @throws IllegalArgumentException
	 *             if not an entity or entity is not managed
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 */
	public void refresh(final Object entity);

	// TODO JPA 2.0 refresh with lock mode
	// TODO JPA 2.0 refresh with properties
	// TODO JPA 2.0 refresh with lock mode and properties

	/**
	 * Clear the persistence context, causing all managed entities to become
	 * detached. Changes made to entities that have not been flushed to the
	 * database will not be persisted.
	 */
	public void clear();

	/**
	 * Remove the given entity from the persistence context, causing a managed
	 * entity to become detached. Unflushed changes made to the entity if any
	 * (including removal of the entity), will not be synchronized to the
	 * database. Entities which previously referenced the detached entity will
	 * continue to reference it.
	 * 
	 * @param entity The instance to detach
	 * @throws IllegalArgumentException
	 *             if the instance is not an entity
	 */
	public void detach(Object entity);

	/**
	 * Check if the instance belongs to the current persistence context.
	 * 
	 * @param entity The instance to check
	 * @return True if the instance is managed, false otherwise
	 * @throws IllegalArgumentException
	 *             if not an entity
	 */
	public boolean contains(Object entity);

	/**
	 * Checks consistency of the specified context. </p>
	 * 
	 * The context URI can be {@code null}, which indicates that consistency of
	 * the whole repository should be verified.
	 * 
	 * @param context
	 *            Context URI, can be {@code null}
	 * @return {@code true} if consistent, {@code false} otherwise
	 */
	@NonJPA
	public boolean isConsistent(URI context);

	// TODO JPA 2.0 public LockModeType getLockMode(Object entity)
	// TODO JPA 2.0 setProperty
	// TODO JPA 2.0 getProperties

	/**
	 * Create an instance of Query for executing a Java Persistence query
	 * language statement.
	 * 
	 * @param qlString
	 *            a Java Persistence query string
	 * @return the new query instance
	 * @throws IllegalArgumentException
	 *             if query string is not valid
	 */
	@NonJPA
	public Query createQuery(String qlString);

	// TODO JPA 2.0 TypedQuery<T> createQuery(CriteriaQuery<T> criteriaQuery)

	/**
	 * Creates an instance of query for executing Java persistence query
	 * language statement.
	 * 
	 * @param query
	 *            query string
	 * @param resultClass
	 *            result type
	 * @return the new query instance
	 */
	@NonJPA
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass);

	//
	// /**
	// * Create an instance of Query for executing a named query (in the Java
	// * Persistence query language or in native SQL).
	// *
	// * @param name
	// * the name of a query defined in metadata
	// * @return the new query instance
	// * @throws IllegalArgumentException
	// * if a query has not been defined with the given name
	// */
	// public Query createNamedQuery(String name);
	//
	// TODO JPA 2.0 TypedQuery<T> createNamedQuery(String query, Class<T>
	// resultClass)

	/**
	 * Create an instance of Query for executing a native SPARQL-DL query in
	 * SPARQL syntax.
	 * 
	 * @param sqlString
	 *            a native SPARQL query string
	 * @return the new query instance
	 */
	public Query<List<String>> createNativeQuery(String sqlString);

	/**
	 * Create an instance of Query for executing a native SPARQL-DL query
	 * returning only specific object type.
	 * 
	 * @param sqlString
	 *            a native SQL query string
	 * @param resultClass
	 *            the class of the resulting instance(s)
	 * @return the new query instance
	 */
	public <T> TypedQuery<T> createNativeQuery(String sqlString, Class<T> resultClass);

	// /**
	// * Create an instance of Query for executing a native SQL query.
	// *
	// * @param sqlString
	// * a native SQL query string
	// * @param resultSetMapping
	// * the name of the result set mapping
	// * @return the new query instance
	// */
	// public Query createNativeQuery(String sqlString, String
	// resultSetMapping);

	// /**
	// * Indicate to the EntityManager that a JTA transaction is active. This
	// * method should be called on a JTA application managed EntityManager that
	// * was created outside the scope of the active transaction to associate it
	// * with the current JTA transaction.
	// *
	// * @throws TransactionRequiredException
	// * if there is no transaction.
	// */
	// public void joinTransaction();

	public <T> T unwrap(Class<T> cls);

	/**
	 * Return the underlying provider object for the EntityManager, if
	 * available. The result of this method is implementation specific.
	 */
	public Object getDelegate();

	/**
	 * Close an application-managed EntityManager. After the close method has
	 * been invoked, all methods on the EntityManager instance and any Query
	 * objects obtained from it will throw the IllegalStateException except for
	 * getTransaction and isOpen (which will return false). If this method is
	 * called when the EntityManager is associated with an active transaction,
	 * the persistence context remains managed until the transaction completes.
	 * 
	 * @throws IllegalStateException
	 *             if the EntityManager is container-managed.
	 */
	public void close();

	/**
	 * Determine whether the EntityManager is open.
	 * 
	 * @return true until the EntityManager has been closed.
	 */
	public boolean isOpen();

	/**
	 * Return the resource-level transaction object. The EntityTransaction
	 * instance may be used serially to begin and commit multiple transactions.
	 * 
	 * @return EntityTransaction instance
	 * @throws IllegalStateException
	 *             if invoked on a JTA EntityManager.
	 */
	public EntityTransaction getTransaction();

	/**
	 * @since JPA 2.0
	 */
	public EntityManagerFactory getEntityManagerFactory();

//	/**
//	 * Returns a label for the given IRI. The label is returned with the
//	 * following preference: 1) label in the language specified for the entity
//	 * manager 2) label without language tag 3) any (unspecified) label
//	 *
//	 * @param iri
//	 * @return
//	 */
//	@NonJPA
//	public String getLabel(final String iri);

	/**
	 * Returns a list of repository contexts available to this entity manager.
	 * </p>
	 * 
	 * @return List of repository context URIs
	 */
	@NonJPA
	public List<URI> getContexts();

	// TODO JPA 2.0 public CriteriaBuilder getCriteriaBuilder();
	public Metamodel getMetamodel();

	/**
	 * Sets the transactional ontology as the one which will be used when
	 * processing SPARQL queries. </p>
	 * 
	 * This setting may have significant impact on query results, since changes
	 * made during transaction are propagated to the transactional ontology,
	 * which is private to this persistence context, before commit. The ontology
	 * can even be in an inconsistent state. </p>
	 * 
	 * This is the default setting, unless changed by properties passed on
	 * persistence initialization.
	 * 
	 * @see #setUseBackupOntologyForQueryProcessing()
	 */
	@NonJPA
	public void setUseTransactionalOntologyForQueryProcessing();

	/**
	 * Returns true if the transactional ontology should be used for SPARQL
	 * query processing.
	 * 
	 * @return {@code true} if transactional ontology will be used,
	 *         {@code false} otherwise
	 * @see #setUseTransactionalOntologyForQueryProcessing()
	 */
	@NonJPA
	public boolean useTransactionalOntologyForQueryProcessing();

	/**
	 * Sets the backup ontology as the one which will be used for processing of
	 * SPARQL queries. </p>
	 * 
	 * The backup ontology represents the ontology after the last commit done by
	 * any transaction and therefore can produce different results from those
	 * produced by the transactional ontology, which is private to this
	 * persistence context.
	 * 
	 * @see #setUseTransactionalOntologyForQueryProcessing()
	 */
	@NonJPA
	public void setUseBackupOntologyForQueryProcessing();

	/**
	 * Returns true if the backup (central) ontology should be used for SPARQL
	 * query processing. </p>
	 * 
	 * @return {@code true} if the central ontology will be used, {@code false}
	 *         otherwise
	 * @see #setUseBackupOntologyForQueryProcessing()
	 */
	@NonJPA
	public boolean useBackupOntologyForQueryProcessing();
}
