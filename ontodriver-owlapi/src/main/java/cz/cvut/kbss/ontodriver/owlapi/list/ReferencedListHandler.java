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
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.ArrayList;
import java.util.List;

public class ReferencedListHandler {

    private static final int NEXT_NODE_GENERATION_THRESHOLD = 100;

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

    public List<Axiom<NamedResource>> loadList(ReferencedListDescriptor descriptor) {
        final List<Axiom<NamedResource>> list = new ArrayList<>();
        final ReferencedListIterator<NamedResource> iterator = iterator(descriptor);
        while (iterator.hasNext()) {
            list.add(iterator.next());
        }
        return list;
    }

    public <V> void persistList(ReferencedListValueDescriptor<V> descriptor) {
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(createListAxioms(descriptor)));
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
        final ReferencedListNodeGenerator nodeGenerator = new ReferencedListNodeGenerator(
                descriptor.getListOwner(), descriptor.getNodeContent());
        boolean first = true;
        NamedResource previousNode = descriptor.getListOwner();
        nodeGenerator.setIndex(0);
        for (V value : descriptor.getValues()) {
            if (first) {
                nodeGenerator.setProperty(descriptor.getListProperty());
                previousNode = nodeGenerator.addListNode(previousNode, value);
                first = false;
            } else {
                nodeGenerator.setProperty(descriptor.getNextNode());
                previousNode = nodeGenerator.addListNode(previousNode, value);
            }
        }
        return nodeGenerator.getChanges();
    }

    public <V> void updateList(ReferencedListValueDescriptor<V> descriptor) {
        if (descriptor.getValues().isEmpty()) {
            removeObsoleteNodes(iterator(descriptor));
        } else if (isOrigEmpty(descriptor)) {
            persistList(descriptor);
        } else {
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
        final ReferencedListNodeGenerator nodeGenerator = new ReferencedListNodeGenerator(descriptor.getListOwner(),
                descriptor.getNodeContent());
        nodeGenerator.setProperty(descriptor.getNextNode()); // We know that we are past list head
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

    private class ReferencedListNodeGenerator {

        private final String baseUri;
        private final List<TransactionalChange> changes;
        private final Assertion nodeContentProperty;

        private int index;
        private Assertion property;

        ReferencedListNodeGenerator(NamedResource baseUri, Assertion nodeContent) {
            this.baseUri = baseUri.toString() + "-SEQ_";
            this.changes = new ArrayList<>();
            this.nodeContentProperty = nodeContent;
        }

        private void setIndex(int index) {
            this.index = index;
        }

        private void setProperty(Assertion property) {
            this.property = property;
        }

        private List<TransactionalChange> getChanges() {
            return changes;
        }

        private <V> NamedResource addListNode(NamedResource previousNode, V value) {
            assert property != null;

            final NamedResource node = generateNode();
            final OWLAxiom nodeAxiom = axiomAdapter
                    .toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(previousNode, property, new Value<>(node)));
            changes.add(new MutableAddAxiom(ontology, nodeAxiom));
            changes.add(generateNodeContent(node, value));
            index++;
            return node;
        }

        private <V> TransactionalChange generateNodeContent(NamedResource node, V value) {
            if (nodeContentProperty.getType() == Assertion.AssertionType.OBJECT_PROPERTY) {
                final OWLAxiom valueAxiom = axiomAdapter
                        .toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(node, nodeContentProperty, new Value<>(value)));
                return new MutableAddAxiom(ontology, valueAxiom);
            } else {
                assert nodeContentProperty.getType() == Assertion.AssertionType.DATA_PROPERTY;
                final OWLAxiom valueAxiom = axiomAdapter.toOwlDataPropertyAssertionAxiom(new AxiomImpl<>(node, nodeContentProperty, new Value<>(value)));
                return new MutableAddAxiom(ontology, valueAxiom);
            }
        }

        private NamedResource generateNode() {
            int i = index;
            IRI iri;
            do {
                iri = IRI.create(baseUri + i);
                if (!ontology.containsIndividualInSignature(iri)) {
                    return NamedResource.create(iri.toURI());
                }
                i++;
            }
            while (i < (i + NEXT_NODE_GENERATION_THRESHOLD));
            throw new IdentifierGenerationException(
                    "Unable to generate identifier for sequence node with base " + baseUri);
        }
    }
}
