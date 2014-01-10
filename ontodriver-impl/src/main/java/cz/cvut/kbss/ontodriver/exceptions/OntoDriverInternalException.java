package cz.cvut.kbss.ontodriver.exceptions;

/**
 * A generic exception for OntoDriver internal exceptions.
 * 
 * @author ledvima1
 * 
 */
public class OntoDriverInternalException extends RuntimeException {

	private static final long serialVersionUID = 5625979267518509732L;

	public OntoDriverInternalException() {
	}

	public OntoDriverInternalException(String message) {
		super(message);
	}

	public OntoDriverInternalException(Throwable cause) {
		super(cause);
	}

	public OntoDriverInternalException(String message, Throwable cause) {
		super(message, cause);
	}
}
