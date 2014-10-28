package cz.cvut.kbss.ontodriver.sesame;

import static cz.cvut.kbss.jopa.utils.ErrorUtils.constructNPXMessage;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.Lists;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class SesameConnection implements Connection {

	private SesameAdapter adapter;
	private boolean open;
	private boolean autoCommit;

	private SesameLists lists;

	public SesameConnection(SesameAdapter adapter) {
		assert adapter != null;
		this.adapter = adapter;
		this.lists = new SesameLists(this, adapter);
		this.open = true;
	}

	@Override
	public void close() throws Exception {
		if (!open) {
			return;
		}
		try {
			adapter.close();
		} finally {
			this.open = false;
		}
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void commit() throws OntoDriverException {
		ensureOpen();
		if (autoCommit) {
			return;
		}
		adapter.commit();
	}

	@Override
	public void rollback() throws OntoDriverException {
		ensureOpen();
		if (autoCommit) {
			return;
		}
		adapter.rollback();
	}

	@Override
	public void setAutoCommit(boolean autoCommit) {
		ensureOpen();
		this.autoCommit = autoCommit;
	}

	@Override
	public boolean isAutoCommit() {
		ensureOpen();
		return autoCommit;
	}

	@Override
	public Statement createStatement() throws OntoDriverException {
		ensureOpen();
		return new SesameStatement(adapter.getQueryExecutor());
	}

	@Override
	public PreparedStatement prepareStatement(String sparql) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(sparql, constructNPXMessage("sparql"));
		if (sparql.isEmpty()) {
			throw new IllegalArgumentException("The value for prepared statement cannot be empty.");
		}
		return new SesamePreparedStatement(adapter.getQueryExecutor(), sparql);
	}

	@Override
	public boolean isConsistent(URI context) throws OntoDriverException {
		ensureOpen();
		return adapter.isConsistent(context);
	}

	@Override
	public List<URI> getContexts() throws OntoDriverException {
		ensureOpen();
		return adapter.getContexts();
	}

	@Override
	public URI generateIdentifier(URI classUri) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(classUri, constructNPXMessage("classUri"));
		try {
			return adapter.generateIdentifier(classUri);
		} catch (IdentifierGenerationException e) {
			throw new SesameDriverException(e);
		}
	}

	@Override
	public boolean contains(Axiom<?> axiom, URI context) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(axiom, constructNPXMessage("axiom"));
		return adapter.contains(axiom, context);
	}

	@Override
	public Collection<Axiom<?>> find(AxiomDescriptor descriptor) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage("descriptor"));
		return adapter.find(descriptor);
	}

	@Override
	public void persist(AxiomValueDescriptor descriptor) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage("descriptor"));
		adapter.persist(descriptor);
		commitIfAuto();
	}

	@Override
	public void update(AxiomValueDescriptor descriptor) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage("descriptor"));
		adapter.update(descriptor);
		commitIfAuto();
	}

	@Override
	public void remove(AxiomDescriptor descriptor) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage("descriptor"));
		adapter.remove(descriptor);
		commitIfAuto();
	}

	@Override
	public Lists lists() {
		ensureOpen();
		return lists;
	}

	void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("This connection is closed.");
		}
	}

	void commitIfAuto() throws OntoDriverException {
		if (autoCommit) {
			adapter.commit();
		}
	}
}
