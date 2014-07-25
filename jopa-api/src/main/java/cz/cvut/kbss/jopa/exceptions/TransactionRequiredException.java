package cz.cvut.kbss.jopa.exceptions;

public class TransactionRequiredException extends RuntimeException {

	private static final long serialVersionUID = 1340495489212146877L;

	public TransactionRequiredException() {
	}

	public TransactionRequiredException(String message) {
		super(message);
	}

	public TransactionRequiredException(Throwable cause) {
		super(cause);
	}

	public TransactionRequiredException(String message, Throwable cause) {
		super(message, cause);
	}
}
