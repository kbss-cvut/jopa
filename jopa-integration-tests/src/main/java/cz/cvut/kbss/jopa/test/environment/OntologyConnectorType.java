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
package cz.cvut.kbss.jopa.test.environment;

public enum OntologyConnectorType {
    /**
     * OWL API ontology connector. Name: owlapi
     */
    OWLAPI("owlapi", "cz.cvut.kbss.ontodriver.owlapi.OwlapiDataSource"),
    /**
     * Jena ontology connector. Name: jena
     */
    JENA("jena", "cz.cvut.kbss.ontodriver.jena.JenaDataSource"),
    /**
     * RDF4J ontology connector. Name: rdf4j
     */
    RDF4J("rdf4j", "cz.cvut.kbss.ontodriver.rdf4j.Rdf4jDataSource");

    private final String name;

    private final String dataSource;

    OntologyConnectorType(String name, String dataSource) {
        this.name = name;
        this.dataSource = dataSource;
    }

    public String getName() {
        return name;
    }

    public String getDriverClass() {
        return dataSource;
    }
}
