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

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import cz.cvut.kbss.ontodriver.rdf4j.util.ValueConverter;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

abstract class AbstractListIterator<JT> implements ListIterator<JT> {

    protected final Resource listOwner;
    protected final IRI hasListProperty;
    protected final IRI hasNextProperty;
    protected final IRI context;
    protected final boolean includeInferred;

    protected final Connector connector;
    protected final ValueFactory vf;

    protected final ValueConverter valueConverter;

    public AbstractListIterator(ListDescriptor listDescriptor, Connector connector, ValueFactory vf) {
        this.listOwner = Rdf4jUtils.toRdf4jIri(listDescriptor.getListOwner().getIdentifier(), vf);
        this.hasListProperty = Rdf4jUtils.toRdf4jIri(listDescriptor.getListProperty()
                                                                     .getIdentifier(), vf);
        this.hasNextProperty = Rdf4jUtils.toRdf4jIri(listDescriptor.getNextNode().getIdentifier(), vf);
        this.context = Rdf4jUtils.toRdf4jIri(listDescriptor.getContext(), vf);
        this.includeInferred = listDescriptor.getListProperty().isInferred();
        this.connector = connector;
        this.vf = vf;
        this.valueConverter = new ValueConverter(vf);
    }

    protected Set<IRI> contexts() {
        return context != null ? Collections.singleton(context) : Collections.emptySet();
    }

    protected void checkSuccessorMax(Collection<Statement> stmts, IRI property) {
        // We don't mind the same statement multiple times, it could have been added during transaction
        if (new HashSet<>(stmts).size() > 1) {
            throw icViolatedException(property, stmts.size());
        }
    }

    protected IntegrityConstraintViolatedException icViolatedException(IRI property, int count) {
        return new IntegrityConstraintViolatedException("Invalid number of values found for assertion " + property + ". Expected 1, got " + count);
    }

    protected void checkObjectIsResource(Statement stmt) {
        if (!stmt.getObject().isResource()) {
            throw new IntegrityConstraintViolatedException(
                    "Invalid property value. Expected object property value, got literal.");
        }
    }
}
