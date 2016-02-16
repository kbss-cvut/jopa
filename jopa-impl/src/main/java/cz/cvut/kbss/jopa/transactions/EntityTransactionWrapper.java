package cz.cvut.kbss.jopa.transactions;

import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

public class EntityTransactionWrapper extends TransactionWrapperImpl {

	private EntityTransaction entityTransaction;

	public EntityTransactionWrapper(AbstractEntityManager entityManger) {
		super(entityManger);
	}

	@Override
	public Object checkForTransaction() {
		if (entityTransaction != null && entityTransaction.isActive()) {
			return entityTransaction;
		}
		return null;
	}

	@Override
	public void registerUOWWithTransaction(UnitOfWork uow) {
		// throw new
		// OWLPeristenceTransactionException("Calling JTA method in not-JTA environment");

	}

	public EntityTransaction getTransaction() {
		if (entityTransaction == null) {
			entityTransaction = new EntityTransactionImpl(this);
		}
		return entityTransaction;
	}

	void begin() {
		setTransactionUOW(getEntityManager().getCurrentPersistenceContext());
	}
}
