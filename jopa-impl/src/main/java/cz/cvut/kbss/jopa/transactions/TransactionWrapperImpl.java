package cz.cvut.kbss.jopa.transactions;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.transactions.TransactionWrapper;

public abstract class TransactionWrapperImpl implements TransactionWrapper {

	protected EntityManager entityManager = null;
	
	protected UnitOfWork transactionUOW;
	
	public TransactionWrapperImpl(EntityManager entityManger) {
		this.entityManager = entityManger;
	}
	
	public void clear() {
		if (transactionUOW != null) {
			transactionUOW.clear();
		}
	}
	
	/**
	 * Check if there is an active transaction running. Returns the transaction object
	 * or null, if there is none.
	 * @return Object
	 */
	public abstract Object checkForTransaction();
	
	/**
	 * Register the given UnitOfWork with the current transaction.
	 * @param uow UnitOfWork
	 */
	public abstract void registerUOWWithTransaction(UnitOfWork uow);

	public UnitOfWork getTransactionUOW() {
		return transactionUOW;
	}

	public void setTransactionUOW(UnitOfWork transactionUOW) {
		this.transactionUOW = transactionUOW;
	}
	
	public EntityManager getEntityManager() {
		return this.entityManager;
	}

}
