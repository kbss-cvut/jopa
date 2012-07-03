package cz.cvut.kbss.jopa.transactions;

/**
 * This interface enables JTA transactions and EntityTransactions
 * to be handled uniformly.
 * 
 * @author kidney
 *
 */
public interface TransactionWrapper {
	public abstract javax.persistence.EntityTransaction getTransaction();
}
