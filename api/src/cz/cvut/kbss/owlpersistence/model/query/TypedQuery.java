package cz.cvut.kbss.owlpersistence.model.query;

import java.util.List;

import javax.transaction.TransactionRequiredException;

public interface TypedQuery<ResultElement> extends Query<ResultElement> {
	/**
	 * Execute a SELECT query and return the query results as a typed List.
	 * 
	 * @return a list of the results
	 * @throws IllegalStateException
	 *             if called for a Java
	 * 
	 *             Persistence query language UPDATE or DELETE statement
	 * @throws QueryTimeoutException
	 *             if the query execution exceeds
	 * 
	 *             the query timeout value set and only the statement is
	 * 
	 *             rolled back
	 * @throws TransactionRequiredException
	 *             if a lock mode has
	 * 
	 *             been set and there is no transaction
	 * @throws PessimisticLockException
	 *             if pessimistic locking
	 * 
	 *             fails and the transaction is rolled back
	 * @throws LockTimeoutException
	 *             if pessimistic locking
	 * 
	 *             fails and only the statement is rolled back
	 * @throws PersistenceException
	 *             if the query execution exceeds
	 * 
	 *             the query timeout value set and the transaction
	 * 
	 *             is rolled back
	 */
	List<ResultElement> getResultList();

}
