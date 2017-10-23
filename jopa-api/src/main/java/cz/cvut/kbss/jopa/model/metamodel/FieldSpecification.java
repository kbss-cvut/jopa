/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.model.annotations.FetchType;

public interface FieldSpecification<X, E> {

    /**
     * Return the managed type representing the type in which the attribute was
     * declared.
     *
     * @return declaring type
     */
    ManagedType<X> getDeclaringType();

    /**
     * Return the Java type of the represented attribute.
     *
     * @return Java type
     */
    Class<E> getJavaType();

    /**
     * Return the java.lang.reflect.Member for the represented attribute.
     *
     * @return corresponding java.lang.reflect.Member
     */
    @NonJPA
    java.lang.reflect.Field getJavaField();

    /**
     * Whether the association is lazily loaded or must be eagerly fetched.
     *
     * @return Fetch type of the field specification
     */
    @NonJPA
    FetchType getFetchType();

    /**
     * Whether this field can contain inferred data.
     *
     * @return Whether field is inferred
     * @see #includeExplicit()
     */
    boolean isInferred();

    /**
     * If this field is inferred, can it contain explicit data as well?
     *
     * @return Whether inferred field can contain explicit knowledge
     * @see #isInferred()
     */
    boolean includeExplicit();

    /**
     * Whether values of this field cannot be modified.
     *
     * @return Read only status of this field
     */
    boolean isReadOnly();

    /**
     * Return the name of the attribute.
     *
     * @return name
     */
    String getName();
}