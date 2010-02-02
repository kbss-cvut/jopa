package cz.cvut.kbss.owlpersistence.owlapi;

import cz.cvut.kbss.owlpersistence.model.EntityTransaction;

public class EntityTransactionImpl implements EntityTransaction {

	private boolean rollbackOnly;

	private boolean active;

	private AbstractEntityManagerImpl em;

	public EntityTransactionImpl(final AbstractEntityManagerImpl o) {
		this.em = o;
	}

	private void ensureActive() {
		if (isActive()) {
			throw new IllegalStateException();
		}
	}

	private void ensureInactive() {
		if (!isActive()) {
			throw new IllegalStateException();
		}
	}

	@Override
	public void begin() {
		ensureInactive();

		// TODO
	}

	@Override
	public synchronized void commit() {
		ensureActive();
		em.flush();
	}

	@Override
	public synchronized boolean getRollbackOnly() {
		ensureActive();
		return rollbackOnly;
	}

	@Override
	public synchronized boolean isActive() {
		return active;
	}

	@Override
	public synchronized void rollback() {
		ensureActive();
		em.clear();
	}

	@Override
	public synchronized void setRollbackOnly() {
		ensureActive();
		this.rollbackOnly = true;
	}
}
