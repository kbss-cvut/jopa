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

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.ArrayList;
import java.util.List;

public class ReferencedListHandler {

    static final int NEXT_NODE_GENERATION_THRESHOLD = 100;

    protected final OwlapiAdapter owlapiAdapter;
    protected final AxiomAdapter axiomAdapter;

    protected final OWLOntology ontology;

    protected final OntologySnapshot snapshot;

    public ReferencedListHandler(OwlapiAdapter owlapiAdapter, OntologySnapshot snapshot) {
        this.owlapiAdapter = owlapiAdapter;
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory());
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
    }

    public List<Axiom<?>> loadList(ReferencedListDescriptor descriptor) {
        final List<Axiom<?>> list = new ArrayList<>();
        final ReferencedListIterator<?> iterator = iterator(descriptor);
        while (iterator.hasNext()) {
            list.add(iterator.next());
        }
        return list;
    }

    public <V> void persistList(ReferencedListValueDescriptor<V> descriptor) {
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        verifyNotRdfList(descriptor);
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(createListAxioms(descriptor)));
    }

    /**
     * OWL does no support RDF lists because it uses them internally, and it would not be possible to distinguish
     * internal RDF lists from user-defined ones.
     *
     * @param descriptor Value descriptor
     * @throws IllegalArgumentException When the descriptor describes an RDF list terminated by RDF nil
     */
    private static void verifyNotRdfList(ReferencedListValueDescriptor<?> descriptor) {
        if (descriptor.isTerminatedByNil()) {
            throw new IllegalArgumentException("RDF nil-terminated lists are not supported by the OWL API driver.");
        }
    }

    <V> ReferencedListIterator<V> iterator(ListDescriptor descriptor) {
        assert descriptor instanceof ReferencedListDescriptor;

        final ReferencedListDescriptor desc = (ReferencedListDescriptor) descriptor;
        if (desc.getListProperty().isInferred() || desc.getNextNode().isInferred() ||
                desc.getNodeContent().isInferred()) {
            return new InferredReferencedListIterator<>(desc, snapshot, axiomAdapter);
        } else {
            return new ReferencedListIterator<>(desc, snapshot, axiomAdapter);
        }
    }

    <V> List<TransactionalChange> createListAxioms(ReferencedListValueDescriptor<V> descriptor) {
        final ReferencedListNodeGenerator nodeGenerator = new ReferencedListNodeGenerator(descriptor, axiomAdapter, ontology);
        NamedResource previousNode = descriptor.getListOwner();
        nodeGenerator.setIndex(0);
        for (V value : descriptor.getValues()) {
            previousNode = nodeGenerator.addListNode(previousNode, value);
        }
        return nodeGenerator.getChanges();
    }

    public <V> void updateList(ReferencedListValueDescriptor<V> descriptor) {
        if (descriptor.getValues().isEmpty()) {
            removeObsoleteNodes(iterator(descriptor));
        } else if (isOrigEmpty(descriptor)) {
            persistList(descriptor);
        } else {
            verifyNotRdfList(descriptor);
            mergeLists(descriptor);
        }
    }

    <V> boolean isOrigEmpty(ReferencedListValueDescriptor<V> descriptor) {
        final ReferencedListIterator<V> it = iterator(descriptor);
        return !it.hasNext();
    }

    <V> void addNewNodes(ReferencedListValueDescriptor<V> descriptor, int index, NamedResource lastNode) {
        if (index >= descriptor.getValues().size()) {
            return;
        }
        final ReferencedListNodeGenerator nodeGenerator = new ReferencedListNodeGenerator(descriptor, axiomAdapter, ontology);
        nodeGenerator.setIndex(index);
        for (; index < descriptor.getValues().size(); index++) {
            final V nextValue = descriptor.getValues().get(index);
            lastNode = nodeGenerator.addListNode(lastNode, nextValue);
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(nodeGenerator.getChanges()));
    }

    private <V> void mergeLists(ReferencedListValueDescriptor<V> descriptor) {
        final ReferencedListIterator<V> it = iterator(descriptor);
        final List<V> values = descriptor.getValues();
        final List<TransactionalChange> changes = new ArrayList<>(values.size());
        int i = 0;
        NamedResource lastNode = null;
        while (it.hasNext() && i < values.size()) {
            final V newValue = values.get(i);
            final V currentValue = it.nextValue();
            if (!newValue.equals(currentValue)) {
                changes.addAll(snapshot.applyChanges(it.replaceNode(newValue)));
            }
            lastNode = it.getCurrentNode();
            i++;
        }
        owlapiAdapter.addTransactionalChanges(changes);
        assert lastNode != null;
        removeObsoleteNodes(it);
        addNewNodes(descriptor, i, lastNode);
    }

    private <V> void removeObsoleteNodes(ReferencedListIterator<V> iterator) {
        if (!iterator.hasNext()) {
            return;
        }
        final List<TransactionalChange> changes = new ArrayList<>();
        while (iterator.hasNext()) {
            iterator.next();
            changes.addAll(iterator.removeWithoutReconnect());
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(changes));
    }

}
