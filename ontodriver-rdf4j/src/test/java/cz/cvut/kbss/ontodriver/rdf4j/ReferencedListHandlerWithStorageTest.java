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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ReferencedListHandlerWithStorageTest
        extends ListHandlerWithStorageTestBase<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private static final String NODE_CONTENT_PROPERTY =
            "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContents";

    @BeforeEach
    public void setUp() throws Exception {
        connector = repositoryProvider.createConnector(false);
        this.handler = new ReferencedListHandler(connector, connector.getValueFactory());
        connector.begin();
    }

    @Test
    public void persistsReferencedList() throws Exception {
        final ReferencedListValueDescriptor descriptor = initValues(8);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        handler.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    private ReferencedListValueDescriptor initValues(int count) {
        final ReferencedListValueDescriptor desc = new ReferencedListValueDescriptor(OWNER,
                Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NODE_CONTENT_PROPERTY), false));
        for (int i = 0; i < count; i++) {
            desc.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#EntityA_" + i));
        }
        return desc;
    }

    @Override
    Collection<Axiom<NamedResource>> generateAxiomsForList(ReferencedListValueDescriptor listDescriptor) {
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
        final ReferencedListValueDescriptor descriptor = initValues(0);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        handler.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    @Test
    public void updatesListByPersistingValuesWhenTheOriginalWasEmpty() throws Exception {
        final ReferencedListValueDescriptor descriptor = initValues(5);
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

        final ReferencedListValueDescriptor updated = initValues(0);
        handler.updateList(updated);
        connector.commit();
        connector.begin();
        assertTrue(handler.loadList(updated).isEmpty());
    }

    private ReferencedListValueDescriptor persistOriginalList() throws Exception {
        final ReferencedListValueDescriptor original = initValues(10);
        handler.persistList(original);
        connector.commit();
        connector.begin();
        assertFalse(handler.loadList(original).isEmpty());
        return original;
    }

    @Test
    public void updatesListByAppendingSeveralNewValues() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (NamedResource r : original.getValues()) {
            updated.addValue(r);
        }
        for (int i = 0; i < 5; i++) {
            updated.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Appended_" + i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByRemovingSeveralValuesFromTheEnd() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size() / 2; i++) {
            updated.addValue(original.getValues().get(i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByReplacingSomeElements() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size(); i++) {
            if (i % 2 != 0) {
                updated.addValue(NamedResource
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Modified_" + i));
            } else {
                updated.addValue(original.getValues().get(i));
            }
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByPrependingSeveralNewElements() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (int i = 0; i < 4; i++) {
            updated.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Prepended_" + i));
        }
        for (NamedResource elem : original.getValues()) {
            updated.addValue(elem);
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByReplacingTheWholeListWithNewElements() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size() + 2; i++) {
            updated.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Replacement_" + i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByRemovingSomeOfTheElements() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size(); i++) {
            if (i % 2 != 0) {
                updated.addValue(original.getValues().get(i));
            }
        }
        updateAndCheck(updated);
    }

}
