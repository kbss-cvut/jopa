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

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
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
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.ArrayList;
import java.util.List;

public class SimpleListHandler {

    protected final OwlapiAdapter owlapiAdapter;
    protected final AxiomAdapter axiomAdapter;

    protected final OWLOntology ontology;

    protected final OntologySnapshot snapshot;

    public SimpleListHandler(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.owlapiAdapter = adapter;
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory());
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
    }

    public List<Axiom<NamedResource>> loadList(SimpleListDescriptor descriptor) {
        final List<Axiom<NamedResource>> list = new ArrayList<>();
        final SimpleListIterator iterator = iterator(descriptor);
        while (iterator.hasNext()) {
            list.add(iterator.next());
        }
        return list;
    }

    public void persistList(SimpleListValueDescriptor descriptor) {
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(createListAxioms(descriptor)));
    }

    SimpleListIterator iterator(ListDescriptor descriptor) {
        if (descriptor.getListProperty().isInferred() || descriptor.getNextNode().isInferred()) {
            return new InferredSimpleListIterator(descriptor, snapshot, axiomAdapter);
        } else {
            return new SimpleListIterator(descriptor, snapshot, axiomAdapter);
        }
    }

    List<TransactionalChange> createListAxioms(SimpleListValueDescriptor descriptor) {
        final List<TransactionalChange> changes = new ArrayList<>(descriptor.getValues().size());
        NamedResource previous = descriptor.getListOwner();
        boolean first = true;
        for (NamedResource item : descriptor.getValues()) {
            final OWLAxiom axiom;
            if (first) {
                axiom = appendNode(previous, descriptor.getListProperty(), item);
                first = false;
            } else {
                axiom = appendNode(previous, descriptor.getNextNode(), item);
            }
            previous = item;
            changes.add(new MutableAddAxiom(ontology, axiom));
        }
        return changes;
    }

    private OWLAxiom appendNode(NamedResource current, Assertion property, NamedResource next) {
        return axiomAdapter.toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(current, property, new Value<>(next)));
    }

    public void updateList(SimpleListValueDescriptor descriptor) {
        if (descriptor.getValues().isEmpty()) {
            removeObsoleteNodes(iterator(descriptor));
        } else if (isOrigEmpty(descriptor)) {
            persistList(descriptor);
        } else {
            mergeLists(descriptor);
        }
    }

    boolean isOrigEmpty(SimpleListValueDescriptor descriptor) {
        final SimpleListIterator it = iterator(descriptor);
        return !it.hasNext();
    }

    private void mergeLists(SimpleListValueDescriptor descriptor) {
        final SimpleListIterator it = iterator(descriptor);
        final List<NamedResource> values = descriptor.getValues();
        final List<TransactionalChange> changes = new ArrayList<>(values.size());
        int i = 0;
        NamedResource lastNode = null;
        while (it.hasNext() && i < values.size()) {
            final NamedResource newValue = values.get(i);
            final NamedResource currentValue = it.nextValue();
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

    private void removeObsoleteNodes(SimpleListIterator iterator) {
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

    void addNewNodes(SimpleListValueDescriptor descriptor, int index, NamedResource lastNode) {
        if (index >= descriptor.getValues().size()) {
            return;
        }
        final List<TransactionalChange> changes = new ArrayList<>(descriptor.getValues().size() - index);
        for (; index < descriptor.getValues().size(); index++) {
            final NamedResource next = descriptor.getValues().get(index);
            changes.add(new MutableAddAxiom(ontology, appendNode(lastNode, descriptor.getNextNode(), next)));
            lastNode = next;
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(changes));
    }
}
