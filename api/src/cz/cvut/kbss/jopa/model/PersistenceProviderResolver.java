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

/**
 * Determine the list of persistence providers available in the runtime
 * environment.
 * 
 * Implementations must be thread-safe.
 * 
 * Note that the getPersistenceProviders method can potentially be called many
 * times: it is recommended that the implementation of this method make use of
 * caching.
 */
public interface PersistenceProviderResolver {

	/**
	 * Returns a list of the PersdistenceProvider implementations available in
	 * the runtime environment.
	 * 
	 * @return list of the persistence providers available in the environment
	 * 
	 * @see JPA 2.0
	 */
	List<PersistenceProvider> getPersistenceProviders();

	/**
	 * Clear cache of providers
	 * 
	 * @see JPA 2.0
	 */
	void clearCachedProviders();
}
