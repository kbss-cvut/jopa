/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

    private RDF() {
        throw new AssertionError();
    }
}
