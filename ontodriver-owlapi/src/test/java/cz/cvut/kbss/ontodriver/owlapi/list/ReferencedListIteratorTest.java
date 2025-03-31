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

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiConsumer;

import static cz.cvut.kbss.ontodriver.owlapi.list.ListTestHelper.HAS_CONTENT_PROPERTY;
import static cz.cvut.kbss.ontodriver.owlapi.list.ListTestHelper.HAS_LIST_PROPERTY;
import static cz.cvut.kbss.ontodriver.owlapi.list.ListTestHelper.HAS_NEXT_PROPERTY;
import static cz.cvut.kbss.ontodriver.owlapi.list.ReferencedListTestHelper.SEQUENCE_NODE_SUFFIX;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ReferencedListIteratorTest extends OwlapiListIteratorBase {

    private ReferencedListDescriptor descriptor;

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        final OWLNamedIndividual individual = snapshot.getDataFactory()
                                                      .getOWLNamedIndividual(IRI.create(ListTestHelper.SUBJECT.getIdentifier()));
        this.testHelper = new ReferencedListTestHelper(snapshot, individual, ListTestHelper.SUBJECT.toString());
        this.descriptor = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST,
                ListTestHelper.HAS_NEXT, ListTestHelper.HAS_CONTENT);
        this.iterator = iterator();
    }

    @Override
    OwlapiListIterator<NamedResource> iterator() {
        return new ReferencedListIterator<>(descriptor, snapshot, axiomAdapter);
    }

    @Test
    void nextValueAllowsMultipleNodeContentStatementsWhenTheyRepresentTranslationsOfString() {
        initMultilingualStringList(List.of(new Translations(Map.of(
                "en", "One",
                "cs", "Jedna"
        ))));
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST,
                ListTestHelper.HAS_NEXT, Assertion.createDataPropertyAssertion(URI.create(HAS_CONTENT_PROPERTY), false));
        final OwlapiListIterator<Translations> sut = new ReferencedListIterator<>(descriptor, snapshot, axiomAdapter);
        assertDoesNotThrow(sut::nextValue);
    }

    private void initMultilingualStringList(List<Translations> list) {
        final OWLDataFactory dataFactory = snapshot.getDataFactory();
        final OWLOntology ontology = snapshot.getOntology();
        final OWLObjectProperty hasList = dataFactory.getOWLObjectProperty(IRI.create(HAS_LIST_PROPERTY));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(HAS_NEXT_PROPERTY));
        final OWLDataProperty hasContent = dataFactory.getOWLDataProperty(IRI.create(HAS_CONTENT_PROPERTY));
        OWLObjectProperty next = hasList;
        int i = 0;
        final String sequenceNodeBase = ListTestHelper.SUBJECT.getIdentifier() + SEQUENCE_NODE_SUFFIX;
        OWLNamedIndividual previousNode = dataFactory.getOWLNamedIndividual(SUBJECT);
        OWLNamedIndividual node = dataFactory.getOWLNamedIndividual(IRI.create(sequenceNodeBase + i));
        for (Translations mls : list) {
            snapshot.getOntologyManager()
                    .addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(next, previousNode, node));
            for (Map.Entry<String, String> e : mls.getValue().entrySet()) {
                snapshot.getOntologyManager()
                        .addAxiom(ontology, dataFactory.getOWLDataPropertyAssertionAxiom(hasContent, node, dataFactory.getOWLLiteral(e.getValue(), e.getKey())));
            }
            previousNode = node;
            node = dataFactory.getOWLNamedIndividual(IRI.create(sequenceNodeBase + i));
            next = hasNext;
        }
    }

    @Test
    void nextValueReturnsMultilingualStringWhenNodeContentAreTranslationsOfString() {
        final Translations mls = new Translations(Map.of(
                "en", "One",
                "cs", "Jedna"
        ));
        initMultilingualStringList(List.of(mls));
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST,
                ListTestHelper.HAS_NEXT, Assertion.createDataPropertyAssertion(URI.create(HAS_CONTENT_PROPERTY), false));
        final OwlapiListIterator<Translations> sut = new ReferencedListIterator<>(descriptor, snapshot, axiomAdapter);

        assertEquals(mls, sut.nextValue());
    }

    @Test
    void replaceNodeReplacesAllTranslationAxiomsWhenNodeContainsTranslationsOfString() {
        final Translations oldMls = new Translations(Map.of(
                "en", "One",
                "cs", "Jedna"
        ));
        initMultilingualStringList(List.of(oldMls));
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST,
                ListTestHelper.HAS_NEXT, Assertion.createDataPropertyAssertion(URI.create(HAS_CONTENT_PROPERTY), false));
        final OwlapiListIterator<Translations> sut = new ReferencedListIterator<>(descriptor, snapshot, axiomAdapter);
        final Translations newMls = new Translations(Map.of(
                "en", "New one",
                "cs", "Nová jedna"
        ));
        sut.next();
        final List<TransactionalChange> result = sut.replaceNode(newMls);
        final BiConsumer<String, String> axiomPresenceCheck = (lang, val) -> assertTrue(result.stream().anyMatch(change -> {
            final List<OWLOntologyChange> ontologyChanges = change.toOwlChanges(snapshot.getOntology());
            for (OWLOntologyChange c : ontologyChanges) {
                final OWLAxiom ax = c.getAxiom();
                assert ax instanceof OWLDataPropertyAssertionAxiom;
                final OWLDataPropertyAssertionAxiom dpa = (OWLDataPropertyAssertionAxiom) ax;
                final OWLLiteral lit = dpa.getObject();
                if (Objects.equals(lang, lit.getLang()) && Objects.equals(val, lit.getLiteral())) {
                    return true;
                }
            }
            return false;
        }));
        oldMls.getValue().forEach(axiomPresenceCheck);
        newMls.getValue().forEach(axiomPresenceCheck);
    }
}
