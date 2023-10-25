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

    private SparqlConstants() {
        throw new AssertionError();
    }
}
