package cz.cvut.kbss.jopa.transactions;


import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

public class EntityTransactionWrapper extends TransactionWrapperImpl {
	
	protected javax.persistence.EntityTransaction entityTransaction;

	public EntityTransactionWrapper(EntityManager entityManger) {
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
		//throw new OWLPeristenceTransactionException("Calling JTA method in not-JTA environment");

	}


	public javax.persistence.EntityTransaction getTransaction() {
		if (entityTransaction == null) {
			entityTransaction = new EntityTransactionImpl(this);
		}
		return entityTransaction;
	}


}
