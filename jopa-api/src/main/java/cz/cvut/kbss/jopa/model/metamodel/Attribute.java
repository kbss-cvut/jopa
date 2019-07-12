/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
        /**
         * Corresponds to datatype properties
         */
        @NonJPA
        DATA,

        /**
         * Corresponds to object properties
         */
        @NonJPA
        OBJECT,

        /**
         * Corresponds to annotation properties
         */
        @NonJPA
        ANNOTATION;
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
    java.lang.reflect.Member getJavaMember();

    /**
     * Return the java.lang.reflect.Member for the represented attribute.
     *
     * @return corresponding java.lang.reflect.Member
     */
    @NonJPA
    IRI getIRI();

    /**
     * Return the set of cascade types specified for this attribute.
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
     * Whether the attribute can be without value.
     * <p>
     * Note that if {@link #getConstraints()} contains {@link ParticipationConstraint}s, this property should be
     * ignored.
     *
     * @return boolean indicating whether the attribute can be empty
     */
    boolean isNonEmpty();

    /**
     * Does the attribute contain values in lexical form.
     * <p>
     * Note that lexical form attributes are effectively read only.
     * <p>
     * Applies only to datatype and annotation properties, object properties cannot be in lexical form.
     *
     * @return Boolean indicating whether the attribute contains values in lexical form
     */
    boolean isLexicalForm();

    /**
     * Does the attribute represent simple literals.
     * <p>
     * Simple literals are stored as {@code xsd:string}, i.e., strings without language tag.
     * <p>
     * Applies only to datatype and annotation properties, object properties cannot be simple literals.
     *
     * @return Boolean indicating whether the attribute represents simple literals.
     */
    boolean isSimpleLiteral();

    /**
     * Returns participation constraints specified for this attribute.
     *
     * @return Array of participation constraints
     */
    ParticipationConstraint[] getConstraints();
}
