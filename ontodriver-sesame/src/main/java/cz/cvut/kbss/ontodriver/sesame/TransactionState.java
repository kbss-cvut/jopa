package cz.cvut.kbss.ontodriver.sesame;

/**
 * Basic transactional states.
 * 
 * @author ledvima1
 * 
 */
public enum TransactionState {
	ACTIVE, PARTIALLY_COMMITTED, COMMITTED, FAILED, ABORTED
}
