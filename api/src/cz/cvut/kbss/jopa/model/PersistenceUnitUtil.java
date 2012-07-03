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

public interface PersistenceUnitUtil {

	public boolean isLoaded(Object entity, String attributeName);

	public boolean isLoaded(Object entity);

	/**
	 * Return the id of the entity. A generated id is not guaranteed to be
	 * available until after the database insert has occurred. Returns null if
	 * the entity does not yet have an id.
	 * 
	 * @param entity
	 *            entity instance
	 * @return id of the entity
	 * @throws IllegalArgumentException
	 *             if the object is found not to be an entity
	 */
	public Object getIdentifier(Object entity);
}
