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

import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.Cache;

public interface EntityManagerFactory {
	/**
	 * Create a new application-managed EntityManager. This method returns a new
	 * EntityManager instance each time it is invoked. The isOpen method will
	 * return true on the returned instance.
	 * 
	 * @return entity manager instance
	 * @throws IllegalStateException
	 *             if the entity manager factory has been closed
	 */
	public EntityManager createEntityManager();

	/**
	 * Create a new EntityManager with the specified Map of properties. This
	 * method returns a new EntityManager instance each time it is invoked. The
	 * isOpen method will return true on the returned instance.
	 */
	public EntityManager createEntityManager(Map<String, String> map);

	// TODO JPA 2.0 getCriteriaBuilder

	/**
	 * Return an instance of Metamodel interface for access to the metamodel of
	 * the persistence unit.
	 * 
	 * @return Metamodel instance
	 * @throws IllegalStateException
	 *             if the entity manager factory has been closed
	 */
	public Metamodel getMetamodel();

	/**
	 * Close the factory, releasing any resources that it holds. After a factory
	 * instance is closed, all methods invoked on it will throw an
	 * IllegalStateException, except for isOpen, which will return false. Once
	 * an EntityManagerFactory has been closed, all its entity managers are
	 * considered to be in the closed state.
	 */
	public void close();

	/**
	 * Indicates whether the factory is open. Returns true until the factory has
	 * been closed.
	 * 
	 * @return true if the entity manager is open
	 * @throws IllegalStateException
	 *             if the entity manager factory has been closed
	 */
	public boolean isOpen();

	public Map<String, String> getProperties();

	/**
	 * Access the cache that is associated with the entity manager factory (the
	 * "second level cache").
	 * 
	 * @return instance of the Cache interface
	 * @throws IllegalStateException
	 *             if the entity manager factory has been closed
	 */
	public Cache getCache();

	public PersistenceUnitUtil getPersistenceUnitUtil();
}
