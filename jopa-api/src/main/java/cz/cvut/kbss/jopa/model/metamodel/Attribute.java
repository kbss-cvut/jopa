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
        ANNOTATION
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
     * Returns the {@link IRI} identifier of the property mapped by this attribute.
     *
     * @return Property IRI
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
    default boolean isAssociation() {
        return getPersistentAttributeType().equals(PersistentAttributeType.OBJECT);
    }

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
     * Applies only to datatype and annotation properties, object properties cannot be in lexical form.
     *
     * @return Boolean indicating whether the attribute contains values in lexical form
     */
    boolean isLexicalForm();

    /**
     * Gets the explicitly specified identifier of the attribute datatype.
     * <p>
     * Note that the returned value may be {@code null} in case the datatype is not explicitly specified and automatic
     * datatype resolution provided by the underlying OntoDriver is used. {@code null} is also returned for object property
     * attributes, as this does not apply to them.
     *
     * @return Datatype identifier, possibly {@code null}
     */
    String getDatatype();

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
     * Indicates whether a language is specified for this attribute.
     * <p>
     * Note that language applies only to String-based data or annotation property attribute values, for which {@code
     * rdfs:langString} values will be read and its language tag compared to the one required.
     * <p>
     * Also note that if the attribute is a simple literal or in lexical form only, it has no language and this method
     * will return false.
     *
     * @return Boolean indicating whether language is specified for this attribute
     */
    boolean hasLanguage();

    /**
     * Gets the language configured for this attribute.
     * <p>
     * Note that language applies only to String-based data or annotation property attribute values, for which {@code
     * rdfs:langString} values will be read and its language tag compared to the one required.
     * <p>
     * If no language is specified directly for the attribute, persistence unit-level language setting is used.
     *
     * @return Language configured for this attribute, {@code null} if none is set
     * @see #hasLanguage()
     */
    String getLanguage();

    /**
     * Returns participation constraints specified for this attribute.
     *
     * @return Array of participation constraints
     */
    ParticipationConstraint[] getConstraints();
}
