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

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

abstract class AbstractListIterator implements ListIterator {

    protected final Resource listOwner;
    protected final IRI hasListProperty;
    protected final IRI hasNextProperty;
    protected final IRI context;
    protected final boolean includeInferred;

    protected final Connector connector;
    protected final ValueFactory vf;

    public AbstractListIterator(ListDescriptor listDescriptor, Connector connector, ValueFactory vf) {
        this.listOwner = Rdf4jUtils.toRdf4jIri(listDescriptor.getListOwner().getIdentifier(), vf);
        this.hasListProperty = Rdf4jUtils.toRdf4jIri(listDescriptor.getListProperty()
                                                                     .getIdentifier(), vf);
        this.hasNextProperty = Rdf4jUtils.toRdf4jIri(listDescriptor.getNextNode().getIdentifier(), vf);
        this.context = Rdf4jUtils.toRdf4jIri(listDescriptor.getContext(), vf);
        this.includeInferred = listDescriptor.getListProperty().isInferred();
        this.connector = connector;
        this.vf = vf;
    }

    protected Set<IRI> contexts() {
        return context != null ? Collections.singleton(context) : Collections.emptySet();
    }

    protected void checkSuccessorMax(Collection<Statement> stmts, IRI property) {
        // We don't mind the same statement multiple times, it could have been added during transaction
        if (new HashSet<>(stmts).size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Invalid number of values found for assertion " + property
                            + ". Expected 1, got " + stmts.size());
        }
    }

    protected void checkNodeIsResource(Statement stmt) {
        if (!(stmt.getObject() instanceof Resource)) {
            throw new IntegrityConstraintViolatedException(
                    "Invalid property value. Expected object property value, got literal.");
        }
    }

    protected Axiom<NamedResource> createAxiom(Resource subject, Assertion assertion, Resource value) {
        final NamedResource subjectRes = NamedResource.create(subject.stringValue());
        return new AxiomImpl<>(subjectRes, assertion, new Value<>(NamedResource.create(value.stringValue())));
    }
}
