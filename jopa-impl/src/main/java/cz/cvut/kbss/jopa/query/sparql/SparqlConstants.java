/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.sparql;

/**
 * Constants of SPARQL.
 */
public class SparqlConstants {

    /**
     * The {@literal SELECT} keyword.
     */
    public static final String SELECT = "SELECT";

    /**
     * The {@literal WHERE} keyword.
     */
    public static final String WHERE = "WHERE";

    /**
     * The {@literal a} keyword representing the rdf:type IRI.
     */
    public static final String RDF_TYPE_SHORTCUT = "a";

    /**
     * The {@literal UNDEF} keyword.
     */
    public static final String UNDEF = "UNDEF";

    private SparqlConstants() {
        throw new AssertionError();
    }
}
