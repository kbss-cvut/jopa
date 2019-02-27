/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.oom.exceptions.UnpersistedChangeException;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;

import java.lang.reflect.Field;
import java.net.URI;

public interface ObjectOntologyMapper {

    /**
     * Checks whether the storage contains individual with the specified identifier and of the specified type.
     *
     * @param cls        Class representing the individual type
     * @param primaryKey Identifier
     * @param descriptor Descriptor, can specify context
     * @return {@code true} if the ontology contains such individual, {@code false} otherwise
     */
    <T> boolean containsEntity(Class<T> cls, URI primaryKey, Descriptor descriptor);

    /**
     * Loads and reconstructs an entity from the ontology.
     *
     * @param loadingParameters Entity loading parameters
     * @return Reconstructed entity or {@code null} if there is none such
     */
    <T> T loadEntity(LoadingParameters<T> loadingParameters);

    /**
     * Loads a reference to an entity corresponding to the specified parameters.
     * <p>
     * The reference is usually an empty object will attributes being loaded lazily. However, it may be also be
     * retrieved from the cache, in which case its attributes will be loaded.
     *
     * @param loadingParameters Reference loading parameters
     * @param <T>               Entity type
     * @return Loaded entity reference or {@code null} if there is none such
     */
    <T> T loadReference(LoadingParameters<T> loadingParameters);

    /**
     * Loads entity field value and sets it on the specified entity.
     *
     * @param entity     The entity on which the field value will be set
     * @param field      The field to load
     * @param descriptor Descriptor possibly specifying the field context
     */
    <T> void loadFieldValue(T entity, Field field, Descriptor descriptor);

    /**
     * Persists the specified entity into the underlying ontology.
     *
     * @param primaryKey Primary key of the persisted entity, possibly {@code null}
     * @param entity     The entity to persist
     * @param descriptor Descriptor possibly specifying entity and attribute contexts
     */
    <T> void persistEntity(URI primaryKey, T entity, Descriptor descriptor);

    /**
     * Removes entity with specified identifier from the ontology.
     *
     * @param primaryKey Entity identifier
     * @param cls        Entity class
     * @param descriptor Descriptor specifying entity attribute contexts
     */
    <T> void removeEntity(URI primaryKey, Class<T> cls, Descriptor descriptor);

    /**
     * Checks that there are no pending changes in the mapper.
     *
     * @throws UnpersistedChangeException Thrown when there are unpersisted changes
     */
    void checkForUnpersistedChanges();

    /**
     * Sets value of property represented by the specified field to the field's value.
     *
     * @param entity     Entity containing the field
     * @param field      The field to update
     * @param descriptor Optionally specifies context
     */
    <T> void updateFieldValue(T entity, Field field, Descriptor descriptor);
}
