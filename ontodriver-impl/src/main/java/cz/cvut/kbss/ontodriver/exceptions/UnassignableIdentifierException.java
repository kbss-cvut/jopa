package cz.cvut.kbss.ontodriver.exceptions;

public class UnassignableIdentifierException extends RuntimeException {

	private static final long serialVersionUID = -5325132033820448113L;

	public UnassignableIdentifierException() {
	}

	public UnassignableIdentifierException(String message) {
		super(message);
	}

	public UnassignableIdentifierException(Throwable cause) {
		super(cause);
	}

	public UnassignableIdentifierException(String message, Throwable cause) {
		super(message, cause);
	}
}
