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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;
import java.util.Collections;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;

class ReferencedListIterator extends AbstractListIterator {

    private final Property hasContent;
    private final Assertion hasContentAssertion;

    ReferencedListIterator(ReferencedListDescriptor descriptor, StorageConnector connector) {
        super(descriptor, connector);
        this.hasContentAssertion = descriptor.getNodeContent();
        this.hasContent = createProperty(hasContentAssertion.getIdentifier().toString());
    }

    @Override
    Axiom<NamedResource> nextAxiom() {
        final NamedResource value = nextValue();
        final NamedResource node = NamedResource.create(currentNode.getURI());
        return new AxiomImpl<>(node, hasContentAssertion, new Value<>(value));
    }

    @Override
    NamedResource nextValue() {
        resolveNextListNode();
        return NamedResource.create(resolveNodeContent().getURI());
    }

    private Resource resolveNodeContent() {
        final Collection<Statement> contentStatements;
        contentStatements = connector.find(currentNode, hasContent, null, contexts());
        verifyContentValueCount(contentStatements);
        final Statement statement = contentStatements.iterator().next();
        assert statement.getObject().isResource();
        return statement.getObject().asResource();
    }

    private void verifyContentValueCount(Collection<Statement> contentStatements) {
        if (contentStatements.isEmpty()) {
            throw new IntegrityConstraintViolatedException("No content found for list node " + currentNode.getURI());
        }
        if (contentStatements.size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Encountered multiple content values of list node " + currentNode.getURI());
        }
    }

    @Override
    void removeWithoutReconnect() {
        super.removeWithoutReconnect();
        remove(currentNode, hasContent, null);
    }

    @Override
    void replace(Resource replacement) {
        remove(currentNode, hasContent, null);
        final Statement toAdd = createStatement(currentNode, hasContent, replacement);
        connector.add(Collections.singletonList(toAdd), context);
    }
}
