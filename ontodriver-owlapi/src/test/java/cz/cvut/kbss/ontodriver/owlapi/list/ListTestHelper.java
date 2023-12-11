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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

abstract class ListTestHelper {

    public static final String HAS_LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    public static final String HAS_NEXT_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasNext";
    public static final String HAS_CONTENT_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContent";

    static final List<URI> LIST_ITEMS = initListItems();
    static final NamedResource SUBJECT = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa#Owner");
    static final Assertion HAS_LIST = Assertion
            .createObjectPropertyAssertion(URI.create(ListTestHelper.HAS_LIST_PROPERTY), false);
    static final Assertion HAS_NEXT = Assertion
            .createObjectPropertyAssertion(URI.create(ListTestHelper.HAS_NEXT_PROPERTY), false);
    static final Assertion HAS_CONTENT = Assertion
            .createObjectPropertyAssertion(URI.create(ListTestHelper.HAS_CONTENT_PROPERTY), false);

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

    abstract void persistList(List<?> items);

    private static List<URI> initListItems() {
        final List<URI> lst = new ArrayList<>(10);
        for (int i = 0; i < 10; i++) {
            lst.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#ListItem_" + i));
        }
        return lst;
    }

    static void verifyListContent(List<URI> expected, List actual) {
        assertEquals(expected.size(), actual.size());
        for (int i = 0; i < expected.size(); i++) {
            assertInstanceOf(Axiom.class, actual.get(i));
            final Object element = ((Axiom) actual.get(i)).getValue().getValue();
            assertInstanceOf(NamedResource.class, element);
            assertEquals(expected.get(i), ((NamedResource) element).getIdentifier());
        }
    }
}
