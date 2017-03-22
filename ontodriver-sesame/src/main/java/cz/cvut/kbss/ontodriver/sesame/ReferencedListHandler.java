/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

public class ReferencedListHandler extends
        ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private int sequenceCounter = 0;

    ReferencedListHandler(Connector connector, ValueFactory vf) {
        super(connector, vf);
    }

    @Override
    SesameIterator createIterator(ReferencedListDescriptor listDescriptor) throws SesameDriverException {
        return new ReferencedListIterator(listDescriptor, connector, vf);
    }

    IRI createListHead(ReferencedListValueDescriptor listValueDescriptor,
                       Collection<Statement> statements) throws SesameDriverException {
        final IRI owner = owner(listValueDescriptor);
        final IRI hasList = hasList(listValueDescriptor);
        final IRI hasContent = hasContent(listValueDescriptor);
        final IRI context = context(listValueDescriptor);
        final IRI nodeUri = generateSequenceNode(owner, context);
        statements.add(vf.createStatement(owner, hasList, nodeUri, context));
        final IRI nodeContent = sesameIri(listValueDescriptor.getValues().get(0).getIdentifier());
        statements.add(vf.createStatement(nodeUri, hasContent, nodeContent, context));
        return nodeUri;
    }

    private IRI hasContent(ReferencedListDescriptor listDescriptor) {
        return sesameIri(listDescriptor.getNodeContent().getIdentifier());
    }

    private IRI generateSequenceNode(IRI owner, IRI context) throws SesameDriverException {
        final String uriBase = owner.stringValue();
        boolean unique;
        IRI node;
        do {
            node = vf.createIRI(uriBase + "-SEQ_" + sequenceCounter++);
            final Collection<Statement> stmts = connector.findStatements(node, null, null, false,
                    context);
            unique = stmts.isEmpty();
        } while (!unique);
        return node;
    }

    List<Statement> createListRest(IRI headNode, ReferencedListValueDescriptor listValueDescriptor)
            throws SesameDriverException {
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
            final IRI content = sesameIri(it.next().getIdentifier());
            previous = createListNode(owner, hasNext, hasContent, content, context, previous, statements);
        }
        return statements;
    }

    private IRI createListNode(IRI owner, IRI hasNext, IRI hasContent, IRI content, IRI context, Resource previous,
                               Collection<Statement> statements) throws SesameDriverException {
        final IRI node = generateSequenceNode(owner, context);
        statements.add(vf.createStatement(previous, hasNext, node, context));
        statements.add(vf.createStatement(node, hasContent, content, context));
        return node;
    }

    @Override
    void clearList(ReferencedListValueDescriptor listDescriptor) throws SesameDriverException {
        final IRI hasNext = hasNext(listDescriptor);
        final IRI hasContent = hasContent(listDescriptor);
        final boolean includeInferred = listDescriptor.getListProperty().isInferred();
        final IRI context = context(listDescriptor);
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
    SesameIterator iterator(ReferencedListValueDescriptor listDescriptor) throws SesameDriverException {
        return new ReferencedListIterator(listDescriptor, connector, vf);
    }

    @Override
    MergeResult mergeWithOriginalList(ReferencedListValueDescriptor listDescriptor, SesameIterator it)
            throws SesameDriverException {
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
            throws SesameDriverException {
        int i = mergeResult.i;
        Resource previous = mergeResult.previous;
        final IRI owner = owner(listDescriptor);
        final IRI hasNext = hasNext(listDescriptor);
        final IRI hasContent = hasContent(listDescriptor);
        final IRI context = context(listDescriptor);
        assert i > 0;
        final Collection<Statement> toAdd = new ArrayList<>((listDescriptor.getValues().size() - i) * 2);
        while (i < listDescriptor.getValues().size()) {
            final IRI content = sesameIri(listDescriptor.getValues().get(i).getIdentifier());
            previous = createListNode(owner, hasNext, hasContent, content, context, previous, toAdd);
            i++;
        }
        connector.addStatements(toAdd);
    }
}
