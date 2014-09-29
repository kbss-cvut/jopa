package cz.cvut.kbss.jopa.oom.exceptions;

/**
 * Thrown when an unpersisted change is found on commit. </p>
 * 
 * This exception is thrown when the application tries to commit a transaction
 * and there are changes that were neither cascaded nor explicitly applied using
 * the EntityManager.
 * 
 * @author kidney
 * 
 */
public class UnpersistedChangeException extends RuntimeException {

	private static final long serialVersionUID = 5768896830313833357L;

	public UnpersistedChangeException() {
	}

	public UnpersistedChangeException(String message) {
		super(message);
	}

	public UnpersistedChangeException(Throwable cause) {
		super(cause);
	}

	public UnpersistedChangeException(String message, Throwable cause) {
		super(message, cause);
	}
}
