package cz.cvut.kbss.ontodriver.sesame;

import java.net.URI;
import java.util.List;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.MutationAxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class SesameAdapter implements Closeable {

	private final Connector connector;
	private boolean open;

	public SesameAdapter(Connector connector) {
		assert connector != null;

		this.connector = connector;
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		try {
			connector.close();
		} finally {
			this.open = false;
		}
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	void commit() {
		// TODO
	}

	void rollback() {
		// TODO
	}

	boolean isConsistent(URI context) {
		// TODO
		return false;
	}

	List<URI> getContexts() {
		// TODO
		return null;
	}

	List<Axiom> find(AxiomDescriptor axiomDescriptor) {
		// TODO
		return null;
	}

	void persist(MutationAxiomDescriptor axiomDescriptor) {
		// TODO
	}

	void update(MutationAxiomDescriptor axiomDescriptor) {
		// TODO
	}

	void remove(AxiomDescriptor axiomDescriptor) {
		// TODO
	}

	StatementExecutor getQueryExecutor() {
		return connector;
	}
}
