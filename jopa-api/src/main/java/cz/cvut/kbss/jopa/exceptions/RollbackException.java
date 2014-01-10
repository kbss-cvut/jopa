package cz.cvut.kbss.jopa.exceptions;

public class RollbackException extends RuntimeException {

	private static final long serialVersionUID = 8371285315001388603L;

	public RollbackException() {
	}

	public RollbackException(String message) {
		super(message);
	}

	public RollbackException(Throwable cause) {
		super(cause);
	}

	public RollbackException(String message, Throwable cause) {
		super(message, cause);
	}
}
