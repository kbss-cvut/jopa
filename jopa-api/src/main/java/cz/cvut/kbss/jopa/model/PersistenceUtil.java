/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

/**
 * Utility interface between the application and the persistence provider(s).
 * <p>
 * The {@code PersistenceUtil} interface instance obtained from the {@link cz.cvut.kbss.jopa.Persistence} class is used
 * to determine the load state of an entity or entity attribute regardless of which persistence provider in the
 * environment created the entity.
 */
public interface PersistenceUtil {

    /**
     * Determine the load state of a given persistent attribute.
     *
     * @param entity        entity containing the attribute
     * @param attributeName name of attribute whose load state is to be determined
     * @return {@code false} if entity's state has not been loaded or if the attribute state has not been loaded, else
     * {@code true}
     */
    boolean isLoaded(Object entity, String attributeName);

    /**
     * Determine the load state of an entity. This method can be used to determine the load state of an entity passed as
     * a reference. An entity is considered loaded if all attributes for which {@link
     * cz.cvut.kbss.jopa.model.annotations.FetchType#EAGER} has been specified have been loaded.
     * <p>
     * The {@link #isLoaded(Object, String)} method should be used to determine the load state of an attribute. Not
     * doing so might lead to unintended loading of state.
     *
     * @param entity whose load state is to be determined
     * @return {@code false} if the entity has not been loaded, else {@code true}
     */
    boolean isLoaded(Object entity);
}
