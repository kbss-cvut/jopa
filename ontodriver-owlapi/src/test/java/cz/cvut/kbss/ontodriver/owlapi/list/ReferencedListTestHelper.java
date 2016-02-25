/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;

import java.net.URI;
import java.util.List;

public class ReferencedListTestHelper extends ListTestHelper {

    static String SEQUENCE_NODE_SUFFIX = "-SEQ_";

    private final String baseUri;

    ReferencedListTestHelper(OntologySnapshot snapshot, OWLNamedIndividual individual, String baseUri) {
        super(snapshot, individual);
        this.baseUri = baseUri;
    }

    @Override
    public void persistList(List<URI> items) {
        assert items.size() > 0;
        final OWLObjectProperty hasList = dataFactory.getOWLObjectProperty(
                IRI.create(HAS_LIST_PROPERTY));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(HAS_NEXT_PROPERTY));
        final OWLObjectProperty hasContent = dataFactory.getOWLObjectProperty(
                IRI.create(HAS_CONTENT_PROPERTY));
        int i = 0;
        final String sequenceNodeBase = baseUri + "-SEQ_";
        OWLNamedIndividual node = dataFactory.getOWLNamedIndividual(IRI.create(sequenceNodeBase + i));
        manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasList, individual, node));
        manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasContent, node,
                dataFactory.getOWLNamedIndividual(IRI.create(items.get(i)))));
        OWLNamedIndividual previousNode;
        for (i = 1; i < items.size(); i++) {
            previousNode = node;
            node = dataFactory.getOWLNamedIndividual(IRI.create(sequenceNodeBase + i));
            manager.addAxiom(ontology,
                    dataFactory.getOWLObjectPropertyAssertionAxiom(hasNext, previousNode, node));
            manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasContent, node,
                    dataFactory.getOWLNamedIndividual(IRI.create(items.get(i)))));

        }
    }
}
