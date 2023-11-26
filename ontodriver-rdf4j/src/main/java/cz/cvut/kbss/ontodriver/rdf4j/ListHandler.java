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

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

/**
 * Base class for list handlers.
 * <p>
 * List handlers are responsible for loading and persisting lists.
 */
abstract class ListHandler {

    final Connector connector;
    final ValueFactory vf;

    ListHandler(Connector connector, ValueFactory vf) {
        this.connector = connector;
        this.vf = vf;
    }

    boolean isOldListEmpty(Resource owner, IRI hasListProperty, boolean includeInferred,
                                   Set<IRI> contexts) throws Rdf4jDriverException {
        final Collection<Statement> stmts = connector.findStatements(owner, hasListProperty, null,
                includeInferred, contexts);
        return stmts.isEmpty();
    }

    void removeObsoletes(ListIterator<?> it) throws Rdf4jDriverException {
        while (it.hasNext()) {
            it.nextNode();
            it.remove();
        }
    }

    Resource extractListNode(Collection<Statement> stmts, IRI nodeAssertion) {
        if (stmts.size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Invalid number of values found for assertion " + nodeAssertion + ". Expected 1, got " +
                            stmts.size());
        }
        final Value val = stmts.iterator().next().getObject();
        if (!(val instanceof Resource)) {
            throw new IntegrityConstraintViolatedException(
                    "Invalid property value. Expected object property value, got literal.");
        }
        return (Resource) val;
    }

    Set<IRI> contexts(ListDescriptor listDescriptor) {
        final IRI ctx = toRdf4jIri(listDescriptor.getContext());
        return ctx != null ? Collections.singleton(ctx) : Collections.emptySet();
    }

    IRI context(ListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getContext());
    }

    IRI owner(ListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getListOwner().getIdentifier());
    }

    IRI hasList(ListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getListProperty().getIdentifier());
    }

    IRI hasNext(ListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getNextNode().getIdentifier());
    }

    IRI toRdf4jIri(java.net.URI uri) {
        return Rdf4jUtils.toRdf4jIri(uri, vf);
    }


    static final class MergeResult {
        final int i;
        final Resource previous;

        MergeResult(int i, Resource node) {
            this.i = i;
            this.previous = node;
        }
    }
}
