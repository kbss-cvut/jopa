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
package cz.cvut.kbss.ontodriver.rdf4j.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class SimpleListHandler extends ListHandler<SimpleListValueDescriptor> {

    public SimpleListHandler(Connector connector, ValueFactory vf) {
        super(connector, vf);
    }

    /**
     * Loads axioms representing list described by the specified list descriptor.
     *
     * @return Collection of axioms representing sequence values
     * @throws Rdf4jDriverException When storage access error occurs
     */
    public List<Axiom<NamedResource>> loadList(SimpleListDescriptor listDescriptor) throws Rdf4jDriverException {
        final List<Axiom<NamedResource>> axioms = new ArrayList<>();
        final ListIterator<NamedResource> it = new SimpleListIterator(listDescriptor, connector, vf);
        while (it.hasNext()) {
            axioms.add(it.nextAxiom());
        }
        return axioms;
    }

    @Override
    protected IRI createListHead(SimpleListValueDescriptor listValueDescriptor, Collection<Statement> listStatements) {
        final IRI firstNode = toRdf4jIri(listValueDescriptor.getValues().get(0).getIdentifier());
        listStatements.add(vf.createStatement(owner(listValueDescriptor), hasList(listValueDescriptor),
                firstNode, context(listValueDescriptor)));
        return firstNode;
    }

    @Override
    protected List<Statement> createListRest(IRI head, SimpleListValueDescriptor listValueDescriptor) {
        final List<Statement> statements = new ArrayList<>(listValueDescriptor.getValues().size());
        IRI previous = head;
        final IRI nextNodeProp = hasNext(listValueDescriptor);
        final IRI context = context(listValueDescriptor);
        final Iterator<NamedResource> it = listValueDescriptor.getValues().iterator();
        it.next();
        while (it.hasNext()) {
            final IRI object = toRdf4jIri(it.next().getIdentifier());
            statements.add(vf.createStatement(previous, nextNodeProp, object, context));
            previous = object;
        }
        return statements;
    }

    /**
     * We are using this code instead of iterator.remove for performance reasons. The iterator has to reconnect the list
     * for each removed node, which takes a lot of time.
     */
    protected void clearList(SimpleListValueDescriptor listValueDescriptor) throws Rdf4jDriverException {
        final Set<IRI> contexts = contexts(listValueDescriptor);
        final Collection<Statement> toRemove = new ArrayList<>();
        IRI currentProperty = hasList(listValueDescriptor);
        final IRI hasNext = hasNext(listValueDescriptor);
        final boolean includeInferred = listValueDescriptor.getNextNode().isInferred();
        Collection<Statement> stmts;
        Resource subject = owner(listValueDescriptor);
        do {
            stmts = connector.findStatements(subject, currentProperty, null, includeInferred, contexts);
            if (!stmts.isEmpty()) {
                subject = extractListNode(stmts, hasNext);
                toRemove.addAll(stmts);
            }
            currentProperty = hasNext;
        } while (!stmts.isEmpty());
        connector.removeStatements(toRemove);
    }

    protected void mergeList(SimpleListValueDescriptor listDescriptor) throws Rdf4jDriverException {
        final ListIterator<NamedResource> it = iterator(listDescriptor);
        final ListHandler.MergeResult mergeResult = mergeWithOriginalList(listDescriptor, it);
        removeObsoletes(it);
        assert mergeResult.i > 0;
        assert mergeResult.previous != null;
        if (mergeResult.i < listDescriptor.getValues().size()) {
            appendNewNodes(listDescriptor, mergeResult);
        }
    }

    MergeResult mergeWithOriginalList(SimpleListValueDescriptor listDescriptor, ListIterator<NamedResource> it) throws
            Rdf4jDriverException {
        int i = 0;
        Resource node = null;
        while (it.hasNext() && i < listDescriptor.getValues().size()) {
            node = it.nextNode();
            final NamedResource newNode = listDescriptor.getValues().get(i);
            if (!node.stringValue().equals(newNode.getIdentifier().toString())) {
                node = toRdf4jIri(newNode.getIdentifier());
                it.replaceCurrentWith(newNode);
            }
            i++;
        }
        return new MergeResult(i, node);
    }

    void appendNewNodes(SimpleListValueDescriptor listDescriptor, MergeResult mergeResult) throws
            Rdf4jDriverException {
        int i = mergeResult.i;
        final Collection<Statement> toAdd = new ArrayList<>(listDescriptor.getValues().size() - i);
        Resource previous = mergeResult.previous;
        final IRI nextNode = toRdf4jIri(listDescriptor.getNextNode().getIdentifier());
        final IRI context = context(listDescriptor);
        while (i < listDescriptor.getValues().size()) {
            final Resource newNode = toRdf4jIri(listDescriptor.getValues().get(i).getIdentifier());
            final Statement stmt = vf.createStatement(previous, nextNode, newNode, context);
            toAdd.add(stmt);
            previous = newNode;
            i++;
        }
        connector.addStatements(toAdd);
    }

    private ListIterator<NamedResource> iterator(SimpleListValueDescriptor listDescriptor) throws Rdf4jDriverException {
        return new SimpleListIterator(listDescriptor, connector, vf);
    }
}
