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

/**
 * Utility interface between the application and the persistence provider managing the persistence unit.
 * <p>
 * The methods of this interface should only be invoked on entity instances obtained from or managed by entity managers
 * for this persistence unit or on new entity instances.
 */
public interface PersistenceUnitUtil {

    /**
     * Determine the load state of a given persistent attribute of an entity belonging to the persistence unit.
     *
     * @param entity        entity instance containing the attribute
     * @param attributeName name of attribute whose load state is to be determined
     * @return false if entity's state has not been loaded or if the attribute state has not been loaded, else true
     */
    boolean isLoaded(Object entity, String attributeName);

    /**
     * Determine the load state of an entity belonging to the persistence unit.
     * <p>
     * This method can be used to determine the load state of an entity passed as a reference. An entity is considered
     * loaded if all attributes for which {@code FetchType.EAGER} has been specified have been loaded.
     * <p>
     * The {@link #isLoaded(Object, String)} method should be used to determine the load state of an attribute. Not
     * doing so might lead to unintended loading of state.
     *
     * @param entity entity instance whose load state is to be determined
     * @return false if the entity has not been loaded, else true
     */
    boolean isLoaded(Object entity);

    /**
     * Return the id of the entity. A generated id is not guaranteed to be available until after the database insert has
     * occurred. Returns null if the entity does not yet have an id.
     *
     * @param entity entity instance
     * @return id of the entity
     * @throws IllegalArgumentException if the object is found not to be an entity
     */
    Object getIdentifier(Object entity);
}
