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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLPropertyAssertionAxiom;

import java.net.URI;
import java.util.List;

public class ReferencedListTestHelper extends ListTestHelper {

    static final String SEQUENCE_NODE_SUFFIX = "-SEQ_";

    private final String baseUri;

    ReferencedListTestHelper(OntologySnapshot snapshot, OWLNamedIndividual individual, String baseUri) {
        super(snapshot, individual);
        this.baseUri = baseUri;
    }

    @Override
    public void persistList(List<?> items) {
        assert !items.isEmpty();
        final OWLObjectProperty hasList = dataFactory.getOWLObjectProperty(IRI.create(HAS_LIST_PROPERTY));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(HAS_NEXT_PROPERTY));
        int i = 0;
        final String sequenceNodeBase = baseUri + SEQUENCE_NODE_SUFFIX;
        OWLNamedIndividual node = dataFactory.getOWLNamedIndividual(IRI.create(sequenceNodeBase + i));
        manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasList, individual, node));
        manager.addAxiom(ontology, createAssertion(node, HAS_CONTENT_PROPERTY, items.get(i)));
        OWLNamedIndividual previousNode;
        for (i = 1; i < items.size(); i++) {
            previousNode = node;
            node = dataFactory.getOWLNamedIndividual(IRI.create(sequenceNodeBase + i));
            manager.addAxiom(ontology,dataFactory.getOWLObjectPropertyAssertionAxiom(hasNext, previousNode, node));
            manager.addAxiom(ontology, createAssertion(node, HAS_CONTENT_PROPERTY, items.get(i)));
        }
    }

    private OWLPropertyAssertionAxiom<?, ?> createAssertion(OWLNamedIndividual node, String property, Object object) {
        if (object instanceof URI) {
            final OWLObjectProperty hasContent = dataFactory.getOWLObjectProperty(IRI.create(property));
            return dataFactory.getOWLObjectPropertyAssertionAxiom(hasContent, node,
                    dataFactory.getOWLNamedIndividual(IRI.create(object.toString())));
        } else {
            final OWLDataProperty hasContent = dataFactory.getOWLDataProperty(IRI.create(property));
            return dataFactory.getOWLDataPropertyAssertionAxiom(hasContent, node, OwlapiUtils.createOWLLiteralFromValue(object, null));
        }
    }
}
