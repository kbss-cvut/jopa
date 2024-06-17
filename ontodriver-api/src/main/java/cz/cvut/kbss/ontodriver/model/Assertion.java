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
package cz.cvut.kbss.ontodriver.model;

import java.net.URI;
import java.util.Objects;

/**
 * Base assertion axiom class.
 * <p>
 * Defines just whether the assertion uses inferred values and existing types of assertions.
 * <p>
 * The usage of types may seem as not being very object-oriented, but since the hierarchy is fixed (there aren't any
 * other kinds of assertions in ontologies) and since the subclasses essentially don't contain any behavior, we can use
 * this way.
 */
public abstract class Assertion extends NamedResource {

    protected final String language;
    private final boolean hasLanguage;

    private final boolean inferred;

    public enum AssertionType {
        /**
         * PROPERTY assertion is used in cases where we don't know the property type, for instance when loading value of
         * the Properties attribute
         */
        CLASS, PROPERTY, OBJECT_PROPERTY, DATA_PROPERTY, ANNOTATION_PROPERTY
    }

    protected Assertion(URI identifier, boolean isInferred) {
        super(identifier);
        this.inferred = isInferred;
        this.language = null;
        this.hasLanguage = false;
    }

    protected Assertion(URI identifier, String language, boolean isInferred) {
        super(identifier);
        this.inferred = isInferred;
        this.language = language;
        this.hasLanguage = language != null;
    }

    /**
     * Whether this assertion is based on inferred values.
     *
     * @return True if inferred, false otherwise
     */
    public boolean isInferred() {
        return inferred;
    }

    /**
     * Whether this assertion is a class assertion.
     * <p>
     * This is a convenience method, its functionality could be emulated by retrieving this assertion's identifier and
     * checking whether it equals to the rdf:type URI.
     *
     * @return True if this assertion is a class assertion, false otherwise.
     */
    public boolean isClassAssertion() {
        return getIdentifier().equals(ClassAssertion.RDF_TYPE);
    }

    /**
     * Gets type of this assertion.
     *
     * @return Assertion type
     */
    public abstract AssertionType getType();

    /**
     * Gets the language tag carried by this assertion.
     * <p>
     * The language tag applies only to string-based literals.
     *
     * @return Language tag, e.g. {@code en}, can be {@code null}
     */
    public String getLanguage() {
        return language;
    }

    /**
     * Checks whether a language tag was set for this assertion.
     *
     * @return {@code true} if a language tag was set, {@code false} otherwise
     */
    public boolean hasLanguage() {
        return hasLanguage;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Assertion)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        Assertion assertion = (Assertion) o;
        return inferred == assertion.inferred && Objects.equals(language, assertion.language);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), language, inferred);
    }

    @Override
    public String toString() {
        return super.toString() + (inferred ? " - inferred" : " - non-inferred");
    }

    /**
     * Creates new class assertion.
     * <p>
     * Class assertions use the rdf:type identifier.
     *
     * @param isInferred Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createClassAssertion(boolean isInferred) {
        return new ClassAssertion(isInferred);
    }

    /**
     * Creates a property assertion without specifying the assertion identifier.
     * <p>
     * Note that the returned instances have the same identifier throughout one JVM - a randomly generated URI.
     *
     * @param isInferred Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createUnspecifiedPropertyAssertion(boolean isInferred) {
        return new PropertyAssertion(isInferred);
    }

    /**
     * Creates a property assertion without specifying the assertion identifier.
     * <p>
     * Note that the returned instances have the same identifier throughout one JVM - a randomly generated URI.
     *
     * @param language   Language tag, optional
     * @param isInferred Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createUnspecifiedPropertyAssertion(String language, boolean isInferred) {
        return new PropertyAssertion(language, isInferred);
    }

    /**
     * Creates new property assertion without specifying what kind of property it is.
     *
     * @param assertionIdentifier Assertion identifier
     * @param isInferred          Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
        return new PropertyAssertion(assertionIdentifier, isInferred);
    }

    /**
     * Creates new property assertion without specifying what kind of property it is.
     *
     * @param assertionIdentifier Assertion identifier
     * @param language            Language tag. Passing {@code null} explicitly specifies that any language tag is
     *                            supported
     * @param isInferred          Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createPropertyAssertion(URI assertionIdentifier, String language, boolean isInferred) {
        return new PropertyAssertion(assertionIdentifier, language, isInferred);
    }

    /**
     * Creates new object property assertion.
     *
     * @param assertionIdentifier Assertion identifier
     * @param isInferred          Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createObjectPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
        return new ObjectPropertyAssertion(assertionIdentifier, isInferred);
    }

    /**
     * Creates new data property assertion.
     *
     * @param assertionIdentifier Assertion identifier
     * @param isInferred          Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createDataPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
        return new DataPropertyAssertion(assertionIdentifier, isInferred);
    }

    /**
     * Creates new data property assertion.
     *
     * @param assertionIdentifier Assertion identifier
     * @param language            Language tag. Passing {@code null} explicitly specifies that any language tag is
     *                            supported
     * @param isInferred          Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createDataPropertyAssertion(URI assertionIdentifier, String language, boolean isInferred) {
        return new DataPropertyAssertion(assertionIdentifier, language, isInferred);
    }

    /**
     * Creates new annotation property assertion.
     *
     * @param assertionIdentifier Assertion identifier
     * @param isInferred          Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createAnnotationPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
        return new AnnotationPropertyAssertion(assertionIdentifier, isInferred);
    }

    /**
     * Creates new annotation property assertion.
     *
     * @param assertionIdentifier Assertion identifier
     * @param language            Language tag. Passing {@code null} explicitly specifies that any language tag is
     *                            supported
     * @param isInferred          Whether the assertion uses inferred values
     * @return Assertion
     */
    public static Assertion createAnnotationPropertyAssertion(URI assertionIdentifier, String language,
                                                              boolean isInferred) {
        return new AnnotationPropertyAssertion(assertionIdentifier, language, isInferred);
    }
}
