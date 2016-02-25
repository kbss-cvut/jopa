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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

class SimpleListIterator extends AbstractSesameIterator {

	private final SimpleListDescriptor listDescriptor;

	private URI currentProperty;

	private Statement current;
	private Collection<Statement> next;

	public SimpleListIterator(SimpleListDescriptor listDescriptor, Connector connector,
			ValueFactory vf) throws SesameDriverException {
		super(listDescriptor, connector, vf);
		this.listDescriptor = listDescriptor;
		this.currentProperty = hasListProperty;
		init();
	}

	private void init() throws SesameDriverException {
		this.next = connector.findStatements(listOwner, hasListProperty, null, includeInferred,
				context);
	}

	@Override
	public boolean hasNext() {
		return (!next.isEmpty());
	}

	@Override
	public Resource nextNode() throws SesameDriverException {
		nextInternal();
		assert current.getObject() instanceof Resource;

		return (Resource) current.getObject();
	}

	private void nextInternal() throws SesameDriverException {
		if (!hasNext()) {
			throw new IllegalStateException();
		}
		checkSuccessorMax(next, currentProperty);
		this.current = next.iterator().next();
		this.currentProperty = current.getPredicate();
		checkNodeIsResource(current);
		final Resource elem = (Resource) current.getObject();
		this.next = connector.findStatements(elem, hasNextProperty, null, includeInferred, context);
	}

	@Override
	public Resource currentContent() throws SesameDriverException {
		assert current.getObject() instanceof Resource;

		return (Resource) current.getObject();
	}

	@Override
	public Axiom<NamedResource> nextAxiom() throws SesameDriverException {
		nextInternal();
		assert current.getObject() instanceof Resource;

		final Assertion assertion = current.getPredicate() == hasListProperty ? listDescriptor
				.getListProperty() : listDescriptor.getNextNode();
		return createAxiom(current.getSubject(), assertion, (Resource) current.getObject());
	}

	@Override
	public void replaceCurrentWith(NamedResource newNode) throws SesameDriverException {
		assert current.getObject() instanceof Resource;
		final Resource newNodeSesame = vf.createURI(newNode.getIdentifier().toString());
		final List<Statement> toAdd = new ArrayList<>(2);
		final List<Statement> toRemove = new ArrayList<>(2);
		toRemove.add(current);
		final Statement newCurrent = vf.createStatement(current.getSubject(), currentProperty,
				newNodeSesame, context);
		// From the current subject to the new node
		toAdd.add(newCurrent);
		if (hasNext()) {
			toRemove.addAll(next);
			final Statement stmt = next.iterator().next();
			checkNodeIsResource(stmt);
			final Resource nextNode = (Resource) stmt.getObject();
			if (!newNodeSesame.equals(nextNode)) {
				// From the new node to the next node
				final Statement newNext = vf.createStatement(newNodeSesame, hasNextProperty,
						nextNode, context);
				toAdd.add(newNext);
				this.next = Collections.singletonList(newNext);
			} else {
				this.next = connector.findStatements(newNodeSesame, hasNextProperty, null,
						includeInferred, context);
			}
		} else {
			this.next = Collections.emptyList();
		}
		this.current = null;
		connector.removeStatements(toRemove);
		connector.addStatements(toAdd);
	}

	@Override
	public void remove() throws SesameDriverException {
		assert current.getObject() instanceof Resource;
		final Collection<Statement> toRemove = new ArrayList<>(next.size() + 1);
		toRemove.add(current);
		if (hasNext()) {
			toRemove.addAll(next);
			final Statement stmt = next.iterator().next();
			checkNodeIsResource(stmt);
			final Resource nextNode = (Resource) stmt.getObject();
			// Here we use the current property, so that it can also be the
			// hasList property
			final Statement toAdd = vf.createStatement(current.getSubject(), currentProperty,
					nextNode, context);
			this.next = Collections.singletonList(toAdd);
			this.current = null;

			connector.addStatements(next);
		} else {
			this.next = Collections.emptyList();
		}
		connector.removeStatements(toRemove);
	}
}
