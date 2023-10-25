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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.ArrayList;
import java.util.List;

public abstract class ListHandler<D extends ListDescriptor, V extends ListValueDescriptor> {

    protected final OwlapiAdapter owlapiAdapter;
    protected final AxiomAdapter axiomAdapter;

    protected final OWLOntology ontology;

    protected final OntologySnapshot snapshot;

    protected ListHandler(OwlapiAdapter owlapiAdapter, OntologySnapshot snapshot) {
        this.owlapiAdapter = owlapiAdapter;
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory());
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
    }

    public List<Axiom<NamedResource>> loadList(D descriptor) {
        final List<Axiom<NamedResource>> list = new ArrayList<>();
        final OwlapiListIterator iterator = iterator(descriptor);
        while (iterator.hasNext()) {
            list.add(iterator.next());
        }
        return list;
    }

    public void persistList(V descriptor) {
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(createListAxioms(descriptor)));
    }

    abstract OwlapiListIterator iterator(ListDescriptor descriptor);

    abstract List<TransactionalChange> createListAxioms(V descriptor);

    public void updateList(V descriptor) {
        if (descriptor.getValues().isEmpty()) {
            removeObsoleteNodes(iterator(descriptor));
        } else if (isOrigEmpty(descriptor)) {
            persistList(descriptor);
        } else {
            mergeLists(descriptor);
        }
    }

    abstract boolean isOrigEmpty(V descriptor);

    private void mergeLists(V descriptor) {
        final OwlapiListIterator it = iterator(descriptor);
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

    private void removeObsoleteNodes(OwlapiListIterator iterator) {
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

    abstract void addNewNodes(V descriptor, int index, NamedResource lastNode);

    public static ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> getSimpleListHandler(
            OwlapiAdapter adapter, OntologySnapshot snapshot) {
        return new SimpleListHandler(adapter, snapshot);
    }

    public static ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> getReferencedListHandler(
            OwlapiAdapter adapter, OntologySnapshot snapshot) {
        return new ReferencedListHandler(adapter, snapshot);
    }
}
