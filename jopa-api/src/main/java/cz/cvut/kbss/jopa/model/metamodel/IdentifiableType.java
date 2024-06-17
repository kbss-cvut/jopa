/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.NonJPA;

import java.util.Set;

/**
 * Instances of the type IdentifiableType represent entity or mapped superclass
 * types.
 *
 * @param <X>
 *            The represented entity or mapped superclass type.
 */
public interface IdentifiableType<X> extends ManagedType<X> {

    /**
     * Return the attribute that corresponds to the version attribute of the
     * entity or mapped superclass.
     *
     * @param type
     *            the type of the represented version attribute
     * @return version attribute
     * @throws IllegalArgumentException
     *             if version attribute of the given type is not present in the
     *             identifiable type
     */

    <Y> SingularAttribute<? super X, Y> getVersion(Class<Y> type);

    /**
     * Return the attribute that corresponds to the version attribute declared
     * by the entity or mapped superclass.
     *
     * @param type
     *            the type of the represented declared version attribute
     * @return declared version attribute
     * @throws IllegalArgumentException
     *             if version attribute of the type is not declared in the
     *             identifiable type
     */
    <Y> SingularAttribute<X, Y> getDeclaredVersion(Class<Y> type);

    /**
     * Return the identifiable types that corresponds to the most specific mapped
     * superclass or entity extended by the entity or mapped superclass.
     *
     * @return supertype of identifiable type or null if no
     *
     *         such supertype
     */
    Set<? extends IdentifiableType<? super X>> getSupertypes();

    /**
     * Whether the identifiable type has a single id attribute. Returns true for
     * a simple id or embedded id; returns false for an idclass.
     *
     * @return boolean indicating whether the identifiable
     *
     *         type has a single id attribute
     */
    boolean hasSingleIdAttribute();

    /**
     * Whether the identifiable type has a version attribute.
     *
     * @return boolean indicating whether the identifiable
     *
     *         type has a version attribute
     */
    boolean hasVersionAttribute();

    /**
     * Return the attribute that corresponds to the id attribute of the entity or mapped superclass.
     * @return Identifier attribute
     */
    @NonJPA
    Identifier<? super X, ?> getIdentifier();
}
