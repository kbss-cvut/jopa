package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SimpleListIterator implements SesameIterator {

	private final SimpleListDescriptor listDescriptor;

	private final Resource listOwner;
	private final URI hasListProperty;
	private final URI hasNextProperty;
	private final URI context;
	private final boolean includeInferred;

	private URI currentProperty;

	private final Connector connector;
	private final ValueFactory vf;

	private Statement current;
	private Collection<Statement> next;

	public SimpleListIterator(SimpleListDescriptor listDescriptor, Connector connector,
			ValueFactory vf) throws SesameDriverException {
		this.listDescriptor = listDescriptor;
		this.listOwner = SesameUtils.toSesameUri(listDescriptor.getListOwner().getIdentifier(), vf);
		this.hasListProperty = SesameUtils.toSesameUri(listDescriptor.getListProperty()
				.getIdentifier(), vf);
		this.hasNextProperty = SesameUtils.toSesameUri(
				listDescriptor.getNextNode().getIdentifier(), vf);
		this.context = SesameUtils.toSesameUri(listDescriptor.getContext(), vf);
		this.includeInferred = listDescriptor.getListProperty().isInferred();
		this.connector = connector;
		this.vf = vf;
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
	public Resource next() throws SesameDriverException {
		nextInternal();
		return (Resource) current.getObject();
	}

	private void nextInternal() throws SesameDriverException {
		if (!hasNext()) {
			throw new IllegalStateException();
		}
		checkSuccessorCount(next, currentProperty);
		this.current = next.iterator().next();
		this.currentProperty = current.getPredicate();
		checkNodeIsResource(current);
		final Resource elem = (Resource) current.getObject();
		this.next = connector.findStatements(elem, hasNextProperty, null, includeInferred, context);
	}

	@Override
	public Axiom<java.net.URI> nextAxiom() throws SesameDriverException {
		nextInternal();
		assert current.getObject() instanceof Resource;

		final Resource subject = current.getSubject();
		final NamedResource subjectRes = NamedResource.create(subject.stringValue());
		final Resource node = (Resource) current.getObject();
		final java.net.URI nodeUri = java.net.URI.create(node.stringValue());
		final Assertion assertion = current.getPredicate() == hasListProperty ? listDescriptor
				.getListProperty() : listDescriptor.getNextNode();
		return new AxiomImpl<java.net.URI>(subjectRes, assertion, new Value<java.net.URI>(nodeUri));
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
		if (!next.isEmpty()) {
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
		connector.removeStatements(Collections.singleton(current));
		if (hasNext()) {
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
	}

	private void checkSuccessorCount(Collection<Statement> stmts, URI property) {
		if (stmts.size() > 1) {
			throw new IntegrityConstraintViolatedException(
					"Invalid number of values found for assertion " + property
							+ ". Expected 1, got " + next.size());
		}
	}

	private void checkNodeIsResource(Statement stmt) {
		if (!(stmt.getObject() instanceof Resource)) {
			throw new IntegrityConstraintViolatedException(
					"Invalid property value. Expected object property value, got literal.");
		}
	}
}
