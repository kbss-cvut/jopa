package cz.cvut.kbss.jopa.transactions;

import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

public class EntityTransactionImpl implements EntityTransaction {

	private static final Logger LOG = Logger.getLogger(EntityTransactionImpl.class.getName());

	private boolean active = false;

	private boolean rollbackOnly = false;

	private final EntityTransactionWrapper wrapper;

	public EntityTransactionImpl(EntityTransactionWrapper wrapper) {
		super();
		this.wrapper = Objects.requireNonNull(wrapper, ErrorUtils.constructNPXMessage("wrapper"));
	}

	/**
	 * Starts the current transaction.
	 * 
	 * @throws IllegalStateException
	 *             if the transaction is already active
	 */
	@Override
	public void begin() {
		if (isActive()) {
			throw new IllegalStateException("Transaction already active!");
		}
		wrapper.begin();
		this.active = true;
		wrapper.getEntityManager().transactionStarted(this);
		if (LOG.isLoggable(Level.FINE)) {
			LOG.config("EntityTransaction begin.");
		}
	}

	/**
	 * Commit the current transaction.
	 * 
	 * @throws IllegalStateException
	 *             when the transaction is not active
	 */
	@Override
	public void commit() {
		if (!isActive()) {
			throw new IllegalStateException("Cannot commit inactive transaction!");
		}
		try {
			if (LOG.isLoggable(Level.FINER)) {
				LOG.fine("EntityTransaction commit started.");
			}
			if (rollbackOnly) {
				throw new RollbackException("Trying to commit transaction marked as rollback only.");
			} else {
				try {
					wrapper.getTransactionUOW().commit();
				} catch (RuntimeException ex) {
					wrapper.getEntityManager().removeCurrentPersistenceContext();
					throw new RollbackException(ex);
				}
			}
		} finally {
			if (wrapper.getTransactionUOW().shouldReleaseAfterCommit()) {
				wrapper.getEntityManager().removeCurrentPersistenceContext();
			}
			cleanup();
			if (LOG.isLoggable(Level.FINE)) {
				LOG.config("EntityTransaction commit finished.");
			}
		}
	}

	private void cleanup() {
		this.active = false;
		this.rollbackOnly = false;
		wrapper.setTransactionUOW(null);
		wrapper.getEntityManager().transactionFinished(this);
	}

	/**
	 * Roll back the current transaction. Dismiss any changes made.
	 * 
	 * @throws IllegalStateException
	 *             when the transaction is not active
	 */
	@Override
	public void rollback() {
		if (!isActive()) {
			throw new IllegalStateException("Cannot rollback inactive transaction!");
		}
		wrapper.getTransactionUOW().rollback();
		wrapper.getEntityManager().removeCurrentPersistenceContext();
		cleanup();
		if (LOG.isLoggable(Level.FINE)) {
			LOG.config("EntityTransaction rolled back.");
		}
	}

	/**
	 * Mark this transaction as rollback only. I. e. the only possible outcome
	 * of this transaction is rollback.
	 * 
	 * @throws IllegalStateException
	 *             when the transaction is not active
	 */
	@Override
	public void setRollbackOnly() {
		if (!isActive()) {
			throw new IllegalStateException("Cannot set rollbackOnly on inactive transaction!");
		}
		this.rollbackOnly = true;
	}

	/**
	 * Is is this transaction marked as rollbackOnly?
	 * 
	 * @throws IllegalStateException
	 *             when the transacion is not active
	 */
	@Override
	public boolean isRollbackOnly() {
		if (!isActive()) {
			throw new IllegalStateException("Accessing rollbackOnly on inactive transaction!");
		}
		return this.rollbackOnly;
	}

	@Override
	public boolean isActive() {
		return active;
	}

	/**
	 * Roll back any changes if we forgot to commit or roll it back manually
	 */
	@Override
	public void finalize() throws Throwable {
		if (isActive()) {
			rollback();
		}
		super.finalize();
	}

}
