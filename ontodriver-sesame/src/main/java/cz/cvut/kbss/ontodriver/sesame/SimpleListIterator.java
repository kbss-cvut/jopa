package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collection;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class SimpleListIterator implements SesameIterator {

	private final Resource listOwner;
	private final URI hasListProperty;
	private final URI hasNextProperty;
	private final URI context;
	private final boolean includeInferred;

	private URI currentProperty;

	private final Connector connector;

	private Collection<Statement> next;

	public SimpleListIterator(SimpleListDescriptor listDescriptor, Connector connector,
			ValueFactory vf) throws SesameDriverException {
		this.listOwner = SesameUtils.toSesameUri(listDescriptor.getListOwner().getIdentifier(), vf);
		this.hasListProperty = SesameUtils.toSesameUri(listDescriptor.getListProperty()
				.getIdentifier(), vf);
		this.hasNextProperty = SesameUtils.toSesameUri(
				listDescriptor.getNextNode().getIdentifier(), vf);
		this.context = SesameUtils.toSesameUri(listDescriptor.getContext(), vf);
		this.includeInferred = listDescriptor.getListProperty().isInferred();
		this.connector = connector;
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
		final Statement stmt = nextInternal();
		return (Resource) stmt.getObject();
	}

	private Statement nextInternal() throws SesameDriverException {
		if (next.size() > 1) {
			throw new IntegrityConstraintViolatedException(
					"Invalid number of values found for assertion " + currentProperty
							+ ". Expected 1, got " + next.size());
		}
		this.currentProperty = hasNextProperty;
		final Statement stmt = next.iterator().next();
		if (!(stmt.getObject() instanceof Resource)) {
			throw new IntegrityConstraintViolatedException(
					"Invalid property value. Expected object property value, got literal.");
		}
		final Resource elem = (Resource) stmt.getObject();
		this.next = connector.findStatements(elem, hasNextProperty, null, includeInferred, context);
		return stmt;
	}

	@Override
	public Axiom<java.net.URI> nextAxiom() throws SesameDriverException {
		// TODO Auto-generated method stub
		return null;
	}

}
