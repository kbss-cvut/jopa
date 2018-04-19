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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.sessions.Cache;

import java.util.Map;

public interface EntityManagerFactory {
    /**
     * Create a new application-managed EntityManager. This method returns a new
     * EntityManager instance each time it is invoked. The isOpen method will
     * return true on the returned instance.
     *
     * @return entity manager instance
     * @throws IllegalStateException if the entity manager factory has been closed
     */
    EntityManager createEntityManager();

    /**
     * Create a new EntityManager with the specified Map of properties. This
     * method returns a new EntityManager instance each time it is invoked. The
     * isOpen method will return true on the returned instance.
     *
     * @param map properties for entity manager
     * @return entity manager instance
     */
    EntityManager createEntityManager(Map<String, String> map);

    // TODO JPA 2.0 getCriteriaBuilder

    /**
     * Return an instance of Metamodel interface for access to the metamodel of
     * the persistence unit.
     *
     * @return Metamodel instance
     * @throws IllegalStateException if the entity manager factory has been closed
     */
    Metamodel getMetamodel();

    /**
     * Close the factory, releasing any resources that it holds. After a factory
     * instance is closed, all methods invoked on it will throw an
     * IllegalStateException, except for isOpen, which will return false. Once
     * an EntityManagerFactory has been closed, all its entity managers are
     * considered to be in the closed state.
     *
     * @throws IllegalStateException if the entity manager factory has been closed
     */
    void close();

    /**
     * Indicates whether the factory is open. Returns true until the factory has
     * been closed.
     *
     * @return true if the entity manager is open
     * @throws IllegalStateException if the entity manager factory has been closed
     */
    boolean isOpen();

    /**
     * Get the properties and associated values that are in effect for the entity manager factory.
     * <p>
     * Changing the contents of the map does not change the configuration in effect.
     *
     * @return properties
     * @throws IllegalStateException if the entity manager factory has been closed
     */
    Map<String, String> getProperties();

    /**
     * Access the cache that is associated with the entity manager factory (the
     * "second level cache").
     *
     * @return instance of the Cache interface
     * @throws IllegalStateException if the entity manager factory has been closed
     */
    Cache getCache();

    /**
     * Return interface providing access to utility methods for the persistence unit.
     *
     * @return {@code PersistenceUnitUtil} interface
     * @throws IllegalStateException if the entity manager factory has been closed
     */
    PersistenceUnitUtil getPersistenceUnitUtil();

    /**
     * Define the query or typed query as a named query such that future query objects can be created from it using the
     * {@code createNamedQuery} method. Any configuration of the query object (except for actual parameter binding) in
     * effect when the named query is added is retained as part of the named query definition. This includes
     * configuration information such as max results and result set mapping information.
     * <p>
     * When the query is executed, information that can be set by means of the query APIs can be overridden. Information
     * that is overridden does not affect the named query as registered with the entity manager factory, and thus does
     * not affect subsequent query objects created from it by means of the {@code createNamedQuery} method.
     * <p>
     * If a named query of the same name has been previously defined, either statically via metadata or via this method,
     * that query definition is replaced.
     *
     * @param name  name for the query
     * @param query {@code Query} or {@code TypedQuery} object
     * @since JPA 2.1
     */
    void addNamedQuery(String name, Query query);

    /**
     * Return an object of the specified type to allow access to the provider-specific API. If the provider's
     * EntityManagerFactory implementation does not support the specified class, the {@link OWLPersistenceException} is
     * thrown.
     *
     * @param cls The class of the object to be returned. This can be also an implementation of the underlying driver
     * @return an instance of the specified class
     * @throws OWLPersistenceException If the provider does not support the specified class
     */
    <T> T unwrap(Class<T> cls);
}
