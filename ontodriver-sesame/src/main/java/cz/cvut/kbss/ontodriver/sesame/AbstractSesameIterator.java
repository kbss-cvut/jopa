/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collection;
import java.util.HashSet;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

abstract class AbstractSesameIterator implements SesameIterator {

    protected final Resource listOwner;
    protected final URI hasListProperty;
    protected final URI hasNextProperty;
    protected final URI context;
    protected final boolean includeInferred;

    protected final Connector connector;
    protected final ValueFactory vf;

    public AbstractSesameIterator(ListDescriptor listDescriptor, Connector connector,
                                  ValueFactory vf) {
        this.listOwner = SesameUtils.toSesameUri(listDescriptor.getListOwner().getIdentifier(), vf);
        this.hasListProperty = SesameUtils.toSesameUri(listDescriptor.getListProperty()
                                                                     .getIdentifier(), vf);
        this.hasNextProperty = SesameUtils.toSesameUri(
                listDescriptor.getNextNode().getIdentifier(), vf);
        this.context = SesameUtils.toSesameUri(listDescriptor.getContext(), vf);
        this.includeInferred = listDescriptor.getListProperty().isInferred();
        this.connector = connector;
        this.vf = vf;
    }

    protected void checkSuccessorMax(Collection<Statement> stmts, URI property) {
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
