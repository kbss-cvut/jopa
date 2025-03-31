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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;

class ReferencedListIterator<T> extends AbstractListIterator<T> {

    private final Property hasContent;
    private final Assertion hasContentAssertion;

    ReferencedListIterator(ReferencedListDescriptor descriptor, StorageConnector connector) {
        super(descriptor, connector);
        this.hasContentAssertion = descriptor.getNodeContent();
        this.hasContent = createProperty(hasContentAssertion.getIdentifier().toString());
    }

    @Override
    boolean hasNext() {
        return super.hasNext() && !isNextNil();
    }

    private boolean isNextNil() {
        assert cursor != null;
        return !cursor.isEmpty() && RDF.nil.equals(cursor.iterator().next().getObject());
    }

    @Override
    Axiom<T> nextAxiom() {
        final T value = nextValue();
        final NamedResource node = NamedResource.create(currentNode.getURI());
        return new AxiomImpl<>(node, hasContentAssertion, new Value<>(value));
    }

    @Override
    T nextValue() {
        resolveNextListNode();
        final List<RDFNode> content = resolveNodeContent();
        if (content.size() == 1) {
            final RDFNode value = content.get(0);
            return (T) (value.isResource() ? NamedResource.create(value.asResource()
                                                                       .getURI()) : JenaUtils.literalToValue(value.asLiteral()));
        } else {
            final Translations mls = new Translations();
            content.forEach(n -> {
                assert n.isLiteral();
                final Literal lit = n.asLiteral();
                assert lit.getLanguage() != null;
                mls.set(lit.getLanguage(), lit.getString());
            });
            return (T) mls;
        }
    }

    private List<RDFNode> resolveNodeContent() {
        final Collection<Statement> contentStatements;
        contentStatements = connector.find(currentNode, hasContent, null, contexts());
        verifyContentValueCount(contentStatements);
        return contentStatements.stream().map(Statement::getObject).collect(Collectors.toList());
    }

    private void verifyContentValueCount(Collection<Statement> contentStatements) {
        if (contentStatements.isEmpty()) {
            throw icViolatedException(currentNode.getURI(), 0);
        }
        final Set<String> langs = new HashSet<>();
        final Set<Statement> statements = new HashSet<>(contentStatements);
        if (statements.size() == 1) {
            return;
        }
        for (Statement s : statements) {
            if (!s.getObject().isLiteral()) {
                throw icViolatedException(currentNode.getURI(), statements.size());
            }
            final Literal literal = (Literal) s.getObject();
            if (literal.getLanguage() != null && !langs.contains(literal.getLanguage())) {
                langs.add(literal.getLanguage());
            } else {
                throw icViolatedException(currentNode.getURI(), statements.size());
            }
        }
    }

    private static IntegrityConstraintViolatedException icViolatedException(String node, int actualCount) {
        return new IntegrityConstraintViolatedException(
                "Expected exactly one content statement for node <" + node + ">, but got " + actualCount);
    }

    @Override
    void removeWithoutReconnect() {
        super.removeWithoutReconnect();
        remove(currentNode, hasContent, null);
    }

    @Override
    void replace(T replacement) {
        remove(currentNode, hasContent, null);
        final List<Statement> toAdd = ReferencedListHelper.toRdfNodes(replacement, hasContentAssertion)
                                                          .map(n -> createStatement(currentNode, hasContent, n))
                                                          .collect(Collectors.toList());
        connector.add(toAdd, context);
    }
}
