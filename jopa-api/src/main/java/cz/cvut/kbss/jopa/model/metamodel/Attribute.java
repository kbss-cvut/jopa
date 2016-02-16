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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.UnusedJPA;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;

/**
 * Represents an attribute of a Java type.
 *
 * @param <X> The represented type that contains the attribute
 * @param <Y> The type of the represented attribute
 */
public interface Attribute<X, Y> extends FieldSpecification<X, Y> {

    enum PersistentAttributeType {
        // @UnusedJPA
        // @Deprecated
        // MANY_TO_ONE,
        //
        // @UnusedJPA
        // @Deprecated
        // ONE_TO_ONE,
        //
        // /**
        // * correspond to the datatype properties
        // */
        // @UnusedJPA
        // @Deprecated
        // BASIC,

        /**
         * correspond to the object properties
         */
        @NonJPA
        DATA,

        /**
         * correspond to the object properties
         */
        @NonJPA
        OBJECT,

        /**
         * correspond to the object properties
         */
        @NonJPA
        ANNOTATION,

        // @UnusedJPA
        // @Deprecated
        // EMBEDDED,
        //
        // @UnusedJPA
        // @Deprecated
        // MANY_TO_MANY,
        //
        // @UnusedJPA
        // @Deprecated
        // ONE_TO_MANY,
        //
        // @UnusedJPA
        // @Deprecated
        // ELEMENT_COLLECTION
    }

    /**
     * Return the persistent attribute type for the attribute.
     *
     * @return persistent attribute type
     */
    PersistentAttributeType getPersistentAttributeType();

    /**
     * Return the java.lang.reflect.Member for the represented attribute.
     *
     * @return corresponding java.lang.reflect.Member
     */
    @UnusedJPA
    @Deprecated
    java.lang.reflect.Member getJavaMember();

    /**
     * Return the java.lang.reflect.Member for the represented attribute.
     *
     * @return corresponding java.lang.reflect.Member
     */
    @NonJPA
    IRI getIRI();

    /**
     * Return the set of cascadetypes specified for this attribute.
     *
     * @return corresponding array of cascade types. This method returns an empty array if no cascade type is specified.
     * @throws IllegalArgumentException if getPersistentAttributeType() returns DATA or ANNOTATION.
     */
    @NonJPA
    CascadeType[] getCascadeTypes();

    /**
     * Is the attribute an association.
     *
     * @return boolean indicating whether the attribute corresponds to an association
     */
    boolean isAssociation();

    /**
     * Is the attribute collection-valued (represents a Collection, Set, List, or Map).
     *
     * @return boolean indicating whether the attribute is collection-valued
     */
    boolean isCollection();

    /**
     * Whether the attribute can be without value.
     * <p>
     * Note that if {@link #getConstraints()} contains {@link ParticipationConstraint}s, this property should be
     * ignored.
     *
     * @return boolean indicating whether the attribute can be empty
     */
    boolean isNonEmpty();

    ParticipationConstraint[] getConstraints();
}
