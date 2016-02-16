/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.jopa.model;

import java.util.Map;

/**
 * @see JPA 2.0
 */
public interface PersistenceProvider {

    /**
     * Called by Persistence class when an EntityManagerFactory is to be created.
     *
     * @param emName the name of the persistence unit
     * @param map    a Map of properties for use by the persistence provider. These properties specify storage
     *               connection configuration and may also specify additional configuration.
     * @return EntityManagerFactory for the persistence unit, or null if the provider is not the right provider.
     */
    EntityManagerFactory createEntityManagerFactory(String emName, Map<String, String> map);

    // TODO JPA 2.0
    // public EntityManagerFactory
    // createContainerEntityManagerFactory(PersistenceUnitInfo info, Map map);

    /**
     * Return the utility interface implemented by the persistence provider.
     *
     * @return ProviderUtil interface
     */
    ProviderUtil getProviderUtil();
}
