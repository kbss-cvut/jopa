/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.lang.reflect.Field;

/**
 * Objects of this interface are responsible for building clones for UnitOfWork
 * transactions.
 */
public interface CloneBuilder {

    /**
     * Builds clone of the given object.
     *
     * @param original           Object
     * @param cloneConfiguration Configuration for the cloning process
     * @return The clone
     * @throws NullPointerException If {@code original} is {@code null}
     */
    Object buildClone(Object original, CloneConfiguration cloneConfiguration);

    /**
     * Builds a clone of the specified entity reference.
     *
     * It is expected that the specified original is an entity, only its identifier is cloned.
     * @param original Entity
     * @param cloneConfiguration Clone configuration
     * @return The clone
     */
    Object buildReferenceClone(Object original, CloneConfiguration cloneConfiguration);

    /**
     * Builds clone of the given object.
     * <p>
     * This method differs from {@link #buildClone(Object, CloneConfiguration)} in that it
     * accepts another argument which represents the owner of the built clone.
     * This is useful in situations when we are cloning attributes directly, e.g. when lazily loading a field value.
     *
     * @param cloneOwner  The owner of the created clone
     * @param clonedField The field whose value is being cloned
     * @param original    The original to clone
     * @param descriptor  Entity descriptor
     * @return The clone
     * @throws NullPointerException If {@code cloneOwner}, {@code original} or {@code contextUri} is {@code null}
     */
    Object buildClone(Object cloneOwner, Field clonedField, Object original, Descriptor descriptor);

    /**
     * Resets the clone builder.
     * <p>
     * Especially resets the visited objects cache to make sure all the clones are built from scratch and are not
     * affected by the previously built ones.
     */
    void reset();

    /**
     * Removes the specified instance from the clone builder's visited entities cache.
     *
     * @param instance   The instance to remove (original object).
     * @param descriptor Instance descriptor
     */
    void removeVisited(Object instance, Descriptor descriptor);

    /**
     * Merges the changes on clone into the original object.
     *
     * @param changeSet Contains changes to merge
     */
    void mergeChanges(ObjectChangeSet changeSet);
}
