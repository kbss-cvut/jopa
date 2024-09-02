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
package cz.cvut.kbss.jopa.vocabulary;

/**
 * A subset of the RDF vocabulary.
 */
public final class RDF {

    /**
     * RDF vocabulary namespace.
     */
    public static final String NAMESPACE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    /**
     * Typical prefix used for {@link #NAMESPACE}.
     */
    public static final String PREFIX = "rdf";

    /**
     * The {@code rdf:type} property indicates that a resource is a member of a class.
     */
    public static final String TYPE = NAMESPACE + "type";

    /**
     * The {@code rdf:value} property identifies the principal value (usually a string) of a property when the property
     * value is a structured resource.
     */
    public static final String VALUE = NAMESPACE + "value";

    /**
     * The {@code rdf:Statement} class represents statements about the properties of resources.
     *
     * @see #SUBJECT
     * @see #PREDICATE
     * @see #OBJECT
     */
    public static final String STATEMENT = NAMESPACE + "Statement";

    /**
     * The subject of an RDF statement.
     */
    public static final String SUBJECT = NAMESPACE + "subject";

    /**
     * The predicate of an RDF statement.
     */
    public static final String PREDICATE = NAMESPACE + "predicate";

    /**
     * The predicate of an RDF statement.
     */
    public static final String OBJECT = NAMESPACE + "object";

    /**
     * {@code rdf:Property} represents those resources that are RDF properties.
     */
    public static final String PROPERTY = NAMESPACE + "Property";

    /**
     * The class {@code rdf:langString} representing language-tagged string values.
     */
    public static final String LANG_STRING = NAMESPACE + "langString";

    /**
     * The {@code rdf:List} class representing (linked) lists in RDF.
     */
    public static final String LIST = NAMESPACE + "List";

    /**
     * The {@code rdf:first} property for RDF list elements (values).
     */
    public static final String FIRST = NAMESPACE + "first";

    /**
     * The {@code rdf:rest} property for linking RDF list elements.
     */
    public static final String REST = NAMESPACE + "rest";

    /**
     * The {@code rdf:nil} object representing an empty RDF list (used to mark the end of an RDF list).
     */
    public static final String NIL = NAMESPACE + "nil";

    /**
     * The {@code rdf:Bag} class is the class of RDF 'Bag' containers.
     */
    public static final String BAG = NAMESPACE + "Bag";

    /**
     * The {@code rdf:Seq} class is the class of RDF 'Sequence' containers.
     */
    public static final String SEQ = NAMESPACE + "Seq";

    /**
     * The {@code rdf:Alt} class is the class of RDF 'Alternative' containers.
     */
    public static final String ALT = NAMESPACE + "Alt";

    private RDF() {
        throw new AssertionError();
    }
}
