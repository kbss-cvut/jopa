/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import java.net.URI;
import java.util.Set;

/**
 * Provides access to the metamodel of persistent entities in the persistence
 * unit.
 */
public interface Metamodel {
    /**
     * Return the metamodel entity type representing the entity.
     *
     * @param cls the type of the represented entity
     * @return the metamodel entity type
     * @throws IllegalArgumentException if not an entity
     */
    <X> EntityType<X> entity(Class<X> cls);

    /**
     * Return the metamodel managed types.
     *
     * @return the metamodel managed types
     */
    Set<ManagedType<?>> getManagedTypes();

    /**
     * Return the metamodel entity types.
     *
     * @return the metamodel entity types
     */
    Set<EntityType<?>> getEntities();

    /**
     * Get the set of classes that contain inferred attributes. These classes
     * are handled specially since inferred attributes can be influenced by
     * changes to any other attributes in any other entity.
     *
     * @return The set of classes with inferred attributes.
     */
    Set<Class<?>> getInferredClasses();

    /**
     * Gets a set of URIs that should be added to module extraction signature.
     * <p>
     * The returned collection is not modifiable.
     *
     * @return A set of URIs
     */
    Set<URI> getModuleExtractionExtraSignature();

    /**
     * Adds the specified URI to the module extraction signature.
     * <p>
     * Note that a module including the specified URI will be created when a new
     * resource level transaction is started. When that will be is
     * implementation dependent. However it must be guaranteed that all
     * subsequent connections provided by OntoDriver will include the URI in
     * extracted modules.
     *
     * @param uri The URI to add
     * @throws NullPointerException If {@code uri} is {@code null}
     */
    void addUriToModuleExtractionSignature(URI uri);
}
