/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

class ReferencedListIterator extends AbstractListIterator {

    private final ReferencedListDescriptor listDescriptor;

    private final IRI hasContentProperty;

    private IRI currentProperty;

    private Statement currentNode;
    private Statement currentContent;
    private Collection<Statement> next;

    public ReferencedListIterator(ReferencedListDescriptor listDescriptor, Connector connector, ValueFactory vf)
            throws Rdf4jDriverException {
        super(listDescriptor, connector, vf);
        this.listDescriptor = listDescriptor;
        this.hasContentProperty = Rdf4jUtils.toRdf4jIri(listDescriptor.getNodeContent().getIdentifier(), vf);
        this.currentProperty = hasListProperty;
        init();
    }

    private void init() throws Rdf4jDriverException {
        this.next = connector.findStatements(listOwner, hasListProperty, null, includeInferred, contexts());
    }

    @Override
    public boolean hasNext() {
        return !next.isEmpty();
    }

    @Override
    public Resource nextNode() throws Rdf4jDriverException {
        nextInternal();
        return (Resource) currentNode.getObject();
    }

    private void nextInternal() throws Rdf4jDriverException {
        if (!hasNext()) {
            throw new IllegalStateException();
        }
        checkSuccessorMax(next, currentProperty);
        this.currentNode = next.iterator().next();
        this.currentProperty = currentNode.getPredicate();
        checkNodeIsResource(currentNode);
        final Resource elem = (Resource) currentNode.getObject();
        this.currentContent = getNodeContent(elem);
        this.next = connector.findStatements(elem, hasNextProperty, null, includeInferred, contexts());
    }

    private Statement getNodeContent(Resource node) throws Rdf4jDriverException {
        final Collection<Statement> elements = connector.findStatements(node, hasContentProperty,
                null, includeInferred, contexts());
        checkSuccessorMax(elements, hasContentProperty);
        if (elements.isEmpty()) {
            throw new IntegrityConstraintViolatedException("Node " + node + " has no content.");
        }
        final Statement elem = elements.iterator().next();
        checkNodeIsResource(elem);
        return elem;
    }

    @Override
    public Resource currentContent() {
        assert currentContent.getObject() instanceof Resource;
        return (Resource) currentContent.getObject();
    }

    @Override
    public Axiom<NamedResource> nextAxiom() throws Rdf4jDriverException {
        nextInternal();
        assert currentContent.getObject() instanceof Resource;

        return createAxiom(currentContent.getSubject(), listDescriptor.getNodeContent(),
                (Resource) currentContent.getObject());
    }

    @Override
    public void remove() throws Rdf4jDriverException {
        assert currentNode.getObject() instanceof Resource;
        final Collection<Statement> toRemove = new ArrayList<>();
        toRemove.add(currentNode);
        toRemove.add(currentContent);
        if (!next.isEmpty()) {
            toRemove.addAll(next);
            final Statement stmt = next.iterator().next();
            checkNodeIsResource(stmt);
            final Resource nextNode = (Resource) stmt.getObject();
            final Statement connectNext = vf
                    .createStatement(currentNode.getSubject(), currentProperty, nextNode, context);
            this.next = Collections.singleton(connectNext);

            this.currentNode = null;
            this.currentContent = null;
            connector.addStatements(next);
        } else {
            next = Collections.emptyList();
        }
        connector.removeStatements(toRemove);
    }

    @Override
    public void replaceCurrentWith(NamedResource newContent) throws Rdf4jDriverException {
        assert currentNode.getObject() instanceof Resource;
        // We just replace the original content statement with new one
        connector.removeStatements(Collections.singleton(currentContent));
        final Resource node = (Resource) currentNode.getObject();
        final Statement stmt = vf
                .createStatement(node, hasContentProperty, Rdf4jUtils.toRdf4jIri(newContent.getIdentifier(), vf),
                                 context);
        connector.addStatements(Collections.singleton(stmt));
    }

}
