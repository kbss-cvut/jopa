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

import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

/**
 * @see JPA 2.0
 */
public interface PersistenceProvider {

	/**
	 * Called by Persistence class when an EntityManagerFactory is to be
	 * created.
	 * 
	 * @param emName
	 *            the name of the persistence unit
	 * @param map
	 *            a Map of properties for use by the persistence provider. These
	 *            properties may be used to override the values of the
	 *            corresponding elements in the persistence.xml file or specify
	 *            values for properties not specified in the persistence.xml
	 *            (and may be null if no properties are specified).
	 * @return EntityManagerFactory for the persistence unit, or null if the
	 *         provider is not the right provider.
	 */
	public EntityManagerFactory createEntityManagerFactory(String emName,
			Map<String, String> map);

	/**
	 * Called by {@code Persistence} class when an {@code EntityManagerFactory}
	 * is to be created.
	 * 
	 * @param emName
	 *            the name of the persistence unit
	 * @param storageProperties
	 *            List of storage properties which specify information about
	 *            storages the persistence should connect to
	 * @param properties
	 *            a {@code Map} of properties for use by the persistence
	 *            provider. These properties may be used to override the values
	 *            of the corresponding elements in the persistence.xml file or
	 *            specify values for properties not specified in the
	 *            persistence.xml (and may be null if no properties are
	 *            specified).
	 * @return {@code EntityManagerFactory} for the persistence unit, or null if
	 *         the provider is not the right provider.
	 */
	@NonJPA
	public EntityManagerFactory createEntityManagerFactory(String emName,
			List<OntologyStorageProperties> storageProperties,
			Map<String, String> properties);

	// TODO JPA 2.0
	// public EntityManagerFactory
	// createContainerEntityManagerFactory(PersistenceUnitInfo info, Map map);

	/**
	 * Return the utility interface implemented by the persistence provider.
	 * 
	 * @return ProviderUtil interface
	 */
	public ProviderUtil getProviderUtil();
}
