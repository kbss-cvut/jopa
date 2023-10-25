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
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.*;

public class ReferencedListHandler extends
        ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private int sequenceCounter = 0;

    ReferencedListHandler(Connector connector, ValueFactory vf) {
        super(connector, vf);
    }

    @Override
    ListIterator createIterator(ReferencedListDescriptor listDescriptor) throws Rdf4jDriverException {
        return new ReferencedListIterator(listDescriptor, connector, vf);
    }

    @Override
    IRI createListHead(ReferencedListValueDescriptor listValueDescriptor,
                       Collection<Statement> statements) throws Rdf4jDriverException {
        final IRI owner = owner(listValueDescriptor);
        final IRI hasList = hasList(listValueDescriptor);
        final IRI hasContent = hasContent(listValueDescriptor);
        final IRI context = context(listValueDescriptor);
        final IRI nodeUri = generateSequenceNode(owner, context);
        statements.add(vf.createStatement(owner, hasList, nodeUri, context));
        final IRI nodeContent = toRdf4jIri(listValueDescriptor.getValues().get(0).getIdentifier());
        statements.add(vf.createStatement(nodeUri, hasContent, nodeContent, context));
        return nodeUri;
    }

    private IRI hasContent(ReferencedListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getNodeContent().getIdentifier());
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

    @Override
    List<Statement> createListRest(IRI headNode, ReferencedListValueDescriptor listValueDescriptor)
            throws Rdf4jDriverException {
        final IRI owner = owner(listValueDescriptor);
        final IRI hasNext = hasNext(listValueDescriptor);
        final IRI hasContent = hasContent(listValueDescriptor);
        final IRI context = context(listValueDescriptor);
        IRI previous = headNode;
        final List<Statement> statements = new ArrayList<>(
                listValueDescriptor.getValues().size() * 2);
        final Iterator<NamedResource> it = listValueDescriptor.getValues().iterator();
        // Skip the first element, it is already in the head
        it.next();
        while (it.hasNext()) {
            final IRI content = toRdf4jIri(it.next().getIdentifier());
            previous = createListNode(owner, hasNext, hasContent, content, context, previous, statements);
        }
        return statements;
    }

    private IRI createListNode(IRI owner, IRI hasNext, IRI hasContent, IRI content, IRI context, Resource previous,
                               Collection<Statement> statements) throws Rdf4jDriverException {
        final IRI node = generateSequenceNode(owner, context);
        statements.add(vf.createStatement(previous, hasNext, node, context));
        statements.add(vf.createStatement(node, hasContent, content, context));
        return node;
    }

    @Override
    void clearList(ReferencedListValueDescriptor listDescriptor) throws Rdf4jDriverException {
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

    @Override
    ListIterator iterator(ReferencedListValueDescriptor listDescriptor) throws Rdf4jDriverException {
        return new ReferencedListIterator(listDescriptor, connector, vf);
    }

    @Override
    MergeResult mergeWithOriginalList(ReferencedListValueDescriptor listDescriptor, ListIterator it)
            throws Rdf4jDriverException {
        int i = 0;
        Resource node = null;
        while (it.hasNext() && i < listDescriptor.getValues().size()) {
            node = it.nextNode();
            final Resource content = it.currentContent();
            final NamedResource newNode = listDescriptor.getValues().get(i);
            if (!content.stringValue().equals(newNode.getIdentifier().toString())) {
                it.replaceCurrentWith(newNode);
            }
            i++;
        }
        return new MergeResult(i, node);
    }

    @Override
    void appendNewNodes(ReferencedListValueDescriptor listDescriptor, MergeResult mergeResult)
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
            final IRI content = toRdf4jIri(listDescriptor.getValues().get(i).getIdentifier());
            previous = createListNode(owner, hasNext, hasContent, content, context, previous, toAdd);
            i++;
        }
        connector.addStatements(toAdd);
    }
}
