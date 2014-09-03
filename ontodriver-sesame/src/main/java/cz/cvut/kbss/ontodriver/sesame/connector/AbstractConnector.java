package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.Transaction;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

abstract class AbstractConnector implements Connector {

	protected Transaction transaction;
	protected boolean open;

	protected AbstractConnector() {
		this.transaction = new Transaction();
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		this.open = false;
	}

	@Override
	public void begin() throws SesameDriverException {
		transaction.begin();
	}

	protected void verifyTransactionActive() {
		if (!transaction.isActive()) {
			throw new IllegalStateException();
		}
	}
}
