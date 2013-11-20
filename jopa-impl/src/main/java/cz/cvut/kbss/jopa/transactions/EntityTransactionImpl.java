package cz.cvut.kbss.jopa.transactions;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.persistence.RollbackException;

import cz.cvut.kbss.jopa.model.EntityManager;

public class EntityTransactionImpl implements javax.persistence.EntityTransaction {

	private static final Logger LOG = Logger.getLogger(EntityTransactionImpl.class.getName());

	private boolean active = false;

	private boolean rollbackOnly = false;

	private EntityTransactionWrapper wrapper;

	public EntityTransactionImpl(EntityTransactionWrapper wrapper) {
		super();
		this.wrapper = wrapper;
	}

	/**
	 * Starts the current transaction.
	 * 
	 * @throws IllegalStateException
	 *             if the transaction is already active
	 */
	public void begin() {
		if (isActive()) {
			throw new IllegalStateException("Transaction already active!");
		}
		EntityManager em = this.wrapper.getEntityManager();
		this.wrapper.transactionUOW = em.getCurrentPersistenceContext();
		this.active = true;
		em.transactionStarted(this);
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
	public void commit() {
		if (!this.isActive()) {
			throw new IllegalStateException("Cannot commit inactive transaction!");
		}
		try {
			if (LOG.isLoggable(Level.FINER)) {
				LOG.fine("EntityTransaction commit started.");
			}
			if (this.wrapper.transactionUOW != null) {
				if (!this.rollbackOnly) {
					this.wrapper.transactionUOW.commit();
				} else {
					throw new RollbackException(
							"Trying to commit transaction marked as rollback only.");
				}
			}
		} catch (RuntimeException ex) {
			ex.printStackTrace();
			if (this.wrapper.transactionUOW != null) {
				this.wrapper.getEntityManager().removeCurrentPersistenceContext();
				this.wrapper.transactionUOW.release();
				this.wrapper.transactionUOW.getParent().release();
				throw ex;
			}
		} finally {
			this.active = false;
			this.rollbackOnly = false;
			if (wrapper.transactionUOW.shouldReleaseAfterCommit()) {
				this.wrapper.getEntityManager().removeCurrentPersistenceContext();
			}
			this.wrapper.setTransactionUOW(null);
			this.wrapper.getEntityManager().transactionFinished(this);
			if (LOG.isLoggable(Level.FINE)) {
				LOG.config("EntityTransaction commit finished.");
			}
		}
	}

	/**
	 * Roll back the current transaction. Dismiss any changes made.
	 * 
	 * @throws IllegalStateException
	 *             when the transaction is not active
	 */
	public void rollback() {
		if (!this.isActive()) {
			throw new IllegalStateException("Cannot rollback inactive transaction!");
		}
		if (wrapper.getTransactionUOW() != null) {
			this.wrapper.transactionUOW.release();
			this.wrapper.transactionUOW.getParent().release();
		}
		this.active = false;
		this.rollbackOnly = false;
		this.wrapper.getEntityManager().removeCurrentPersistenceContext();
		this.wrapper.setTransactionUOW(null);
		this.wrapper.getEntityManager().transactionFinished(this);
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
	public void setRollbackOnly() {
		if (!this.isActive()) {
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
	public boolean getRollbackOnly() {
		if (!this.isActive()) {
			throw new IllegalStateException("Accessing rollbackOnly on inactive transaction!");
		}
		return this.rollbackOnly;
	}

	public boolean isActive() {
		return this.active;
	}

	/**
	 * Roll back any changes if we forgot to commit or roll it back manually
	 */
	public void finalize() throws Throwable {
		if (this.isActive()) {
			this.rollback();
		}
		super.finalize();
	}

}
