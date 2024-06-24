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
package cz.cvut.kbss.ontodriver.rdf4j.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Vocabulary;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ReferencedListHandlerWithStorageTest extends ListHandlerWithStorageTestBase {

    private static final String NODE_CONTENT_PROPERTY =
            "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContents";

    private ReferencedListHandler handler;

    @BeforeEach
    public void setUp() throws Exception {
        connector = repositoryProvider.createConnector(false);
        this.handler = new ReferencedListHandler(connector, connector.getValueFactory());
        connector.begin();
    }

    @Test
    public void persistsReferencedList() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = initValues(8);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        handler.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    private ReferencedListValueDescriptor<NamedResource> initValues(int count) {
        final ReferencedListValueDescriptor<NamedResource> desc = new ReferencedListValueDescriptor<>(OWNER,
                Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NODE_CONTENT_PROPERTY), false));
        for (int i = 0; i < count; i++) {
            desc.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "EntityA_" + i));
        }
        return desc;
    }

    Collection<Axiom<NamedResource>> generateAxiomsForList(
            ReferencedListValueDescriptor<NamedResource> listDescriptor) {
        final Collection<Axiom<NamedResource>> axioms = new ArrayList<>(listDescriptor.getValues().size());
        if (listDescriptor.getValues().isEmpty()) {
            return axioms;
        }
        int counter = 0;
        final String uriBase = OWNER.getIdentifier().toString();
        for (NamedResource val : listDescriptor.getValues()) {

            NamedResource node = NamedResource.create(uriBase + "-SEQ_" + counter++);
            Axiom<NamedResource> ax = new AxiomImpl<>(node, listDescriptor.getNodeContent(), new Value<>(val));
            axioms.add(ax);
        }
        return axioms;
    }

    @Test
    public void persistsEmptyList() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = initValues(0);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        handler.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    @Test
    public void updatesListByPersistingValuesWhenTheOriginalWasEmpty() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> descriptor = initValues(5);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);
        assertTrue(handler.loadList(descriptor).isEmpty());

        handler.updateList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    @Test
    public void updatesListByClearingAllValues() throws Exception {
        persistOriginalList();

        final ReferencedListValueDescriptor<NamedResource> updated = initValues(0);
        handler.updateList(updated);
        connector.commit();
        connector.begin();
        assertTrue(handler.loadList(updated).isEmpty());
    }

    private ReferencedListValueDescriptor<NamedResource> persistOriginalList() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> original = initValues(10);
        handler.persistList(original);
        connector.commit();
        connector.begin();
        assertFalse(handler.loadList(original).isEmpty());
        return original;
    }

    @Test
    public void updatesListByAppendingSeveralNewValues() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> original = persistOriginalList();

        final ReferencedListValueDescriptor<NamedResource> updated = initValues(0);
        for (NamedResource r : original.getValues()) {
            updated.addValue(r);
        }
        for (int i = 0; i < 5; i++) {
            updated.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "Appended_" + i));
        }
        updateAndCheck(updated);
    }

    void updateAndCheck(ReferencedListValueDescriptor<NamedResource> descriptor) throws Exception {
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);
        handler.updateList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    @Test
    public void updatesListByRemovingSeveralValuesFromTheEnd() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> original = persistOriginalList();

        final ReferencedListValueDescriptor<NamedResource> updated = initValues(0);
        for (int i = 0; i < original.getValues().size() / 2; i++) {
            updated.addValue(original.getValues().get(i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByReplacingSomeElements() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> original = persistOriginalList();

        final ReferencedListValueDescriptor<NamedResource> updated = initValues(0);
        for (int i = 0; i < original.getValues().size(); i++) {
            if (i % 2 != 0) {
                updated.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "Modified_" + i));
            } else {
                updated.addValue(original.getValues().get(i));
            }
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByPrependingSeveralNewElements() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> original = persistOriginalList();

        final ReferencedListValueDescriptor<NamedResource> updated = initValues(0);
        for (int i = 0; i < 4; i++) {
            updated.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "Prepended_" + i));
        }
        for (NamedResource elem : original.getValues()) {
            updated.addValue(elem);
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByReplacingTheWholeListWithNewElements() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> original = persistOriginalList();

        final ReferencedListValueDescriptor<NamedResource> updated = initValues(0);
        for (int i = 0; i < original.getValues().size() + 2; i++) {
            updated.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "Replacement_" + i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByRemovingSomeOfTheElements() throws Exception {
        final ReferencedListValueDescriptor<NamedResource> original = persistOriginalList();

        final ReferencedListValueDescriptor<NamedResource> updated = initValues(0);
        for (int i = 0; i < original.getValues().size(); i++) {
            if (i % 2 != 0) {
                updated.addValue(original.getValues().get(i));
            }
        }
        updateAndCheck(updated);
    }
}
