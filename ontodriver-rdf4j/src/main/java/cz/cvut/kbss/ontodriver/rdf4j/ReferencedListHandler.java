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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.ValueConverter;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class ReferencedListHandler extends ListHandler {

    private int sequenceCounter = 0;

    private final ValueConverter valueConverter;

    ReferencedListHandler(Connector connector, ValueFactory vf) {
        super(connector, vf);
        this.valueConverter = new ValueConverter(vf);
    }

    /**
     * Loads axioms representing list described by the specified list descriptor.
     *
     * @return Collection of axioms representing sequence values
     * @throws Rdf4jDriverException When storage access error occurs
     */
    List<Axiom<NamedResource>> loadList(ReferencedListDescriptor listDescriptor) throws Rdf4jDriverException {
        final List<Axiom<NamedResource>> axioms = new ArrayList<>();
        final ListIterator<?> it = new ReferencedListIterator<>(listDescriptor, connector, vf);
        while (it.hasNext()) {
            axioms.add(it.nextAxiom());
        }
        return axioms;
    }

    /**
     * Persists list values specified by the descriptor.
     * <p>
     * The values are saved in the order in which they appear in the descriptor.
     *
     * @param listValueDescriptor Describes values to persist
     * @throws Rdf4jDriverException When storage access error occurs
     */
    <V> void persistList(ReferencedListValueDescriptor<V> listValueDescriptor) throws Rdf4jDriverException {
        if (listValueDescriptor.getValues().isEmpty()) {
            return;
        }
        final Collection<Statement> statements = new ArrayList<>(listValueDescriptor.getValues().size());
        final IRI head = createListHead(listValueDescriptor, statements);
        statements.addAll(createListRest(head, listValueDescriptor));
        connector.addStatements(statements);
    }

    <V> IRI createListHead(ReferencedListValueDescriptor<V> listValueDescriptor,
                       Collection<Statement> statements) throws Rdf4jDriverException {
        final IRI owner = owner(listValueDescriptor);
        final IRI hasList = hasList(listValueDescriptor);
        final IRI hasContent = hasContent(listValueDescriptor);
        final IRI context = context(listValueDescriptor);
        final IRI nodeUri = generateSequenceNode(owner, context);
        statements.add(vf.createStatement(owner, hasList, nodeUri, context));
        final Value nodeContent = toRdf4jValue(listValueDescriptor.getNodeContent(), listValueDescriptor.getValues().get(0));
        statements.add(vf.createStatement(nodeUri, hasContent, nodeContent, context));
        return nodeUri;
    }

    private IRI hasContent(ReferencedListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getNodeContent().getIdentifier());
    }

    private Value toRdf4jValue(Assertion a, Object value) throws Rdf4jDriverException {
        return valueConverter.toRdf4jValue(a, new cz.cvut.kbss.ontodriver.model.Value<>(value));
    }

    private IRI generateSequenceNode(IRI owner, IRI context) throws Rdf4jDriverException {
        final String uriBase = owner.stringValue();
        boolean unique;
        IRI node;
        do {
            node = vf.createIRI(uriBase + "-SEQ_" + sequenceCounter++);
            final Collection<Statement> stmts = connector.findStatements(node, null, null, false,
                    context != null ? Collections.singleton(context) : Collections.emptySet());
            unique = stmts.isEmpty();
        } while (!unique);
        return node;
    }

    <V> List<Statement> createListRest(IRI headNode, ReferencedListValueDescriptor<V> listValueDescriptor)
            throws Rdf4jDriverException {
        final IRI owner = owner(listValueDescriptor);
        final IRI hasNext = hasNext(listValueDescriptor);
        final IRI hasContent = hasContent(listValueDescriptor);
        final IRI context = context(listValueDescriptor);
        IRI previous = headNode;
        final List<Statement> statements = new ArrayList<>(listValueDescriptor.getValues().size() * 2);
        final Iterator<?> it = listValueDescriptor.getValues().iterator();
        // Skip the first element, it is already in the head
        it.next();
        while (it.hasNext()) {
            final Value content = toRdf4jValue(listValueDescriptor.getNodeContent(), it.next());
            previous = createListNode(owner, hasNext, hasContent, content, context, previous, statements);
        }
        return statements;
    }

    private IRI createListNode(IRI owner, IRI hasNext, IRI hasContent, Value content, IRI context, Resource previous,
                               Collection<Statement> statements) throws Rdf4jDriverException {
        final IRI node = generateSequenceNode(owner, context);
        statements.add(vf.createStatement(previous, hasNext, node, context));
        statements.add(vf.createStatement(node, hasContent, content, context));
        return node;
    }

    /**
     * Updates list with values specified by the descriptor.
     *
     * @param listValueDescriptor Describes the updated values
     * @throws Rdf4jDriverException When storage access error occurs
     */
    <V> void updateList(ReferencedListValueDescriptor<V> listValueDescriptor) throws Rdf4jDriverException {
        if (listValueDescriptor.getValues().isEmpty()) {
            clearList(listValueDescriptor);
        } else if (isOldListEmpty(owner(listValueDescriptor), hasList(listValueDescriptor),
                listValueDescriptor.getListProperty().isInferred(), contexts(listValueDescriptor))) {
            persistList(listValueDescriptor);
        } else {
            mergeList(listValueDescriptor);
        }
    }

    private <V> void clearList(ReferencedListValueDescriptor<V> listDescriptor) throws Rdf4jDriverException {
        final IRI hasNext = hasNext(listDescriptor);
        final IRI hasContent = hasContent(listDescriptor);
        final boolean includeInferred = listDescriptor.getListProperty().isInferred();
        final Set<IRI> context = contexts(listDescriptor);
        Resource previous = owner(listDescriptor);
        IRI currentProperty = hasList(listDescriptor);
        final Collection<Statement> toRemove = new ArrayList<>();
        Collection<Statement> next;
        do {
            next = connector.findStatements(previous, currentProperty, null, includeInferred, context);
            if (!next.isEmpty()) {
                final Resource node = extractListNode(next, currentProperty);
                toRemove.addAll(next);
                toRemove.addAll(connector.findStatements(node, hasContent, null, includeInferred, context));
                previous = node;
            }
            currentProperty = hasNext;
        } while (!next.isEmpty());
        connector.removeStatements(toRemove);
    }

    private <V> void mergeList(ReferencedListValueDescriptor<V> listDescriptor) throws Rdf4jDriverException {
        final ListIterator<V> it = new ReferencedListIterator<>(listDescriptor, connector, vf);
        final ListHandler.MergeResult mergeResult = mergeWithOriginalList(listDescriptor, it);
        removeObsoletes(it);
        assert mergeResult.i > 0;
        assert mergeResult.previous != null;
        if (mergeResult.i < listDescriptor.getValues().size()) {
            appendNewNodes(listDescriptor, mergeResult);
        }
    }

    <V> MergeResult mergeWithOriginalList(ReferencedListValueDescriptor<V> listDescriptor, ListIterator<V> it)
            throws Rdf4jDriverException {
        int i = 0;
        Resource node = null;
        while (it.hasNext() && i < listDescriptor.getValues().size()) {
            node = it.nextNode();
            final Object content = it.currentContent();
            final Object newNode = listDescriptor.getValues().get(i);
            if (!content.equals(newNode)) {
                it.replaceCurrentWith((V) newNode);
            }
            i++;
        }
        return new MergeResult(i, node);
    }

    <V> void appendNewNodes(ReferencedListValueDescriptor<V> listDescriptor, MergeResult mergeResult)
            throws Rdf4jDriverException {
        int i = mergeResult.i;
        Resource previous = mergeResult.previous;
        final IRI owner = owner(listDescriptor);
        final IRI hasNext = hasNext(listDescriptor);
        final IRI hasContent = hasContent(listDescriptor);
        final IRI context = context(listDescriptor);
        assert i > 0;
        final Collection<Statement> toAdd = new ArrayList<>((listDescriptor.getValues().size() - i) * 2);
        while (i < listDescriptor.getValues().size()) {
            final Value content = toRdf4jValue(listDescriptor.getListProperty(), listDescriptor.getValues().get(i));
            previous = createListNode(owner, hasNext, hasContent, content, context, previous, toAdd);
            i++;
        }
        connector.addStatements(toAdd);
    }
}
