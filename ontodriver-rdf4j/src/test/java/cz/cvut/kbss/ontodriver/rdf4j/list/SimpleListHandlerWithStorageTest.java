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
package cz.cvut.kbss.ontodriver.rdf4j.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
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

public class SimpleListHandlerWithStorageTest extends ListHandlerWithStorageTestBase {

    private SimpleListHandler sut;

    @BeforeEach
    public void setUp() throws Exception {
        connector = repositoryProvider.createConnector(false);
        this.sut = new SimpleListHandler(connector, connector.getValueFactory());
        connector.begin();
    }

    @Test
    public void persistsSimpleList() throws Exception {
        final SimpleListValueDescriptor descriptor = initValues(8);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        sut.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, sut.loadList(descriptor));
    }

    private SimpleListValueDescriptor initValues(int count) {
        final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(OWNER,
                Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY), false));
        for (int i = 0; i < count; i++) {
            desc.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "EntityA_" + i));
        }
        return desc;
    }

    Collection<Axiom<NamedResource>> generateAxiomsForList(SimpleListValueDescriptor listDescriptor) {
        final Collection<Axiom<NamedResource>> axioms = new ArrayList<>(listDescriptor.getValues().size());
        if (listDescriptor.getValues().isEmpty()) {
            return axioms;
        }
        NamedResource previous = listDescriptor.getValues().get(0);
        for (NamedResource val : listDescriptor.getValues()) {
            Axiom<NamedResource> ax;
            if (val == previous) {
                ax = new AxiomImpl<>(OWNER, listDescriptor.getListProperty(), new Value<>(val));
            } else {
                ax = new AxiomImpl<>(previous, listDescriptor.getNextNode(), new Value<>(val));
            }
            axioms.add(ax);
            previous = val;
        }
        return axioms;
    }

    @Test
    public void persistsEmptyList() throws Exception {
        final SimpleListValueDescriptor descriptor = initValues(0);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        sut.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, sut.loadList(descriptor));
    }

    @Test
    public void updatesListByPersistingValuesWhenTheOriginalWasEmpty() throws Exception {
        final SimpleListValueDescriptor descriptor = initValues(5);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);
        assertTrue(sut.loadList(descriptor).isEmpty());

        sut.updateList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, sut.loadList(descriptor));
    }

    @Test
    public void updatesListByClearingAllValues() throws Exception {
        persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        sut.updateList(updated);
        connector.commit();
        connector.begin();
        assertTrue(sut.loadList(updated).isEmpty());
    }

    private SimpleListValueDescriptor persistOriginalList() throws Exception {
        final SimpleListValueDescriptor original = initValues(10);
        sut.persistList(original);
        connector.commit();
        connector.begin();
        assertFalse(sut.loadList(original).isEmpty());
        return original;
    }

    @Test
    public void updatesListByAppendingSeveralNewValues() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (NamedResource r : original.getValues()) {
            updated.addValue(r);
        }
        for (int i = 0; i < 5; i++) {
            updated.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "Appended_" + i));
        }
        updateAndCheck(updated);
    }

    void updateAndCheck(SimpleListValueDescriptor descriptor) throws Exception {
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);
        sut.updateList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, sut.loadList(descriptor));
    }

    @Test
    public void updatesListByRemovingSeveralValuesFromTheEnd() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size() / 2; i++) {
            updated.addValue(original.getValues().get(i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByReplacingSomeElements() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
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
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
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
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size() + 2; i++) {
            updated.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "Replacement_" + i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByRemovingSomeOfTheElements() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size(); i++) {
            if (i % 2 != 0) {
                updated.addValue(original.getValues().get(i));
            }
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesByInsertingElementsMultipleTimesInOneTransaction() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updatedFirst = initValues(0);
        for (int i = 0; i < original.getValues().size(); i++) {
            if (original.getValues().size() / 2 == i) {
                updatedFirst.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "added"));
            }
            updatedFirst.addValue(original.getValues().get(i));
        }

        Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(updatedFirst);
        sut.updateList(updatedFirst);
        verifyListContent(axioms, sut.loadList(updatedFirst));

        final SimpleListValueDescriptor updatedSecond = initValues(0);
        for (int i = 0; i < original.getValues().size() - 1; i++) {
            updatedSecond.addValue(original.getValues().get(i));
        }
        updatedSecond.addValue(NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "addedSecond"));
        updatedSecond.addValue(original.getValues().get(original.getValues().size() - 1));

        axioms = generateAxiomsForList(updatedSecond);
        sut.updateList(updatedSecond);
        verifyListContent(axioms, sut.loadList(updatedSecond));
    }
}
