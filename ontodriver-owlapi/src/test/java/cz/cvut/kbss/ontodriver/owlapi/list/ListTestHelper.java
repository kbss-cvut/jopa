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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import java.net.URI;
import java.util.List;

abstract class ListTestHelper {

    public static final String HAS_LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    public static final String HAS_NEXT_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasNext";
    public static final String HAS_CONTENT_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContent";

    final OWLOntology ontology;
    final OWLOntologyManager manager;
    final OWLDataFactory dataFactory;
    final OWLNamedIndividual individual;

    ListTestHelper(OntologySnapshot snapshot, OWLNamedIndividual individual) {
        this.ontology = snapshot.getOntology();
        this.manager = snapshot.getOntologyManager();
        this.dataFactory = snapshot.getDataFactory();
        this.individual = individual;
    }

    abstract void persistList(List<URI> items);
}
