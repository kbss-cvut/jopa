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
package cz.cvut.kbss.ontodriver.rdf4j.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.ListElementStorageHelper;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

class ReferencedListIterator<T> extends AbstractListIterator<T> {

    private final ReferencedListDescriptor listDescriptor;

    private final IRI hasContentProperty;

    private IRI currentProperty;

    private Statement currentNode;
    private Collection<Statement> currentContent;
    private Collection<Statement> next;

    public ReferencedListIterator(ReferencedListDescriptor listDescriptor, RepoConnection connector, ValueFactory vf)
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
        return !next.isEmpty() && !isNextNil();
    }

    private boolean isNextNil() {
        assert next != null;
        return RDF.NIL.equals(next.iterator().next().getObject());
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
        super.checkSuccessorMax(next, currentProperty);
        this.currentNode = next.iterator().next();
        this.currentProperty = currentNode.getPredicate();
        checkObjectIsResource(currentNode);
        final Resource elem = (Resource) currentNode.getObject();
        this.currentContent = getNodeContent(elem);
        this.next = connector.findStatements(elem, hasNextProperty, null, includeInferred, contexts());
    }

    private Collection<Statement> getNodeContent(Resource node) throws Rdf4jDriverException {
        final Collection<Statement> elements = connector.findStatements(node, hasContentProperty,
                null, includeInferred, contexts());
        checkSuccessorMax(elements, hasContentProperty);
        if (elements.isEmpty()) {
            throw new IntegrityConstraintViolatedException("Node " + node + " has no content.");
        }
        return elements;
    }

    @Override
    protected void checkSuccessorMax(Collection<Statement> stmts, IRI property) {
        final Set<String> langs = new HashSet<>();
        // Remove duplicates
        final Set<Statement> statements = new HashSet<>(stmts);
        if (statements.size() == 1) {
            return;
        }
        for (Statement s : statements) {
            if (!s.getObject().isLiteral()) {
                throw icViolatedException(property, statements.size());
            }
            final Literal literal = (Literal) s.getObject();
            if (literal.getLanguage().isPresent() && !langs.contains(literal.getLanguage().get())) {
                langs.add(literal.getLanguage().get());
            } else {
                throw icViolatedException(property, statements.size());
            }
        }
    }

    @Override
    public T currentContent() {
        if (currentContent.size() == 1) {
            return (T) fromRdf4jValue(currentContent.iterator().next().getObject());
        } else {
            final Translations mls = currentContentToMultilingualString();
            return (T) mls;
        }
    }

    private static Object fromRdf4jValue(org.eclipse.rdf4j.model.Value value) {
        return value.isLiteral() ? Rdf4jUtils.getLiteralValue((Literal) value) : NamedResource.create(value.stringValue());
    }

    @Override
    public Axiom<T> nextAxiom() throws Rdf4jDriverException {
        nextInternal();

        if (currentContent.size() == 1) {
            final org.eclipse.rdf4j.model.Value obj = currentContent.iterator().next().getObject();
            return new AxiomImpl(NamedResource.create(currentContent.iterator().next().getSubject().stringValue()),
                    listDescriptor.getNodeContent(),
                    new Value<>(fromRdf4jValue(obj)));
        }
        final Translations mls = currentContentToMultilingualString();
        return new AxiomImpl(NamedResource.create(currentContent.iterator().next().getSubject().stringValue()),
                listDescriptor.getNodeContent(), new Value<>(mls));
    }

    private Translations currentContentToMultilingualString() {
        return ListElementStorageHelper.extractTranslations(currentContent.stream().map(Statement::getObject).toList());
    }

    @Override
    public void remove() throws Rdf4jDriverException {
        assert currentNode.getObject() instanceof Resource;
        final Collection<Statement> toRemove = new ArrayList<>();
        toRemove.add(currentNode);
        toRemove.addAll(currentContent);
        if (!next.isEmpty()) {
            toRemove.addAll(next);
            final Statement stmt = next.iterator().next();
            checkObjectIsResource(stmt);
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
    public void replaceCurrentWith(T newContent) throws Rdf4jDriverException {
        assert currentNode.getObject() instanceof Resource;
        // We just replace the original content statement with new one
        connector.removeStatements(currentContent);
        final Collection<org.eclipse.rdf4j.model.Value> contentValues = new ListElementStorageHelper(valueConverter).toRdf4jValue(listDescriptor.getNodeContent(), newContent);
        final Resource node = (Resource) currentNode.getObject();
        connector.addStatements(contentValues.stream()
                                             .map(v -> vf.createStatement(node, hasContentProperty, v, context))
                                             .collect(Collectors.toList()));
    }
}
