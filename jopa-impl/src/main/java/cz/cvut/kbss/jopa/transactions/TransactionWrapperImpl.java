package cz.cvut.kbss.jopa.transactions;

import cz.cvut.kbss.jopa.owlapi.AbstractEntityManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

public abstract class TransactionWrapperImpl implements TransactionWrapper {

	private final AbstractEntityManager entityManager;

	private UnitOfWork transactionUOW;

	public TransactionWrapperImpl(AbstractEntityManager entityManger) {
		this.entityManager = entityManger;
	}

	/**
	 * Check if there is an active transaction running. Returns the transaction
	 * object or null, if there is none.
	 * 
	 * @return Object
	 */
	public abstract Object checkForTransaction();

	/**
	 * Register the given UnitOfWork with the current transaction.
	 * 
	 * @param uow
	 *            UnitOfWork
	 */
	public abstract void registerUOWWithTransaction(UnitOfWork uow);

	public UnitOfWork getTransactionUOW() {
		return transactionUOW;
	}

	public void setTransactionUOW(UnitOfWork transactionUOW) {
		this.transactionUOW = transactionUOW;
	}

	AbstractEntityManager getEntityManager() {
		return entityManager;
	}
}
