package cz.cvut.kbss.jopa.exceptions;

/**
 * Thrown when an attempt to persist entity with duplicate primary key is made.
 * 
 * @author kidney
 * 
 */
public class OWLEntityExistsException extends RuntimeException {

	private static final long serialVersionUID = 453666323423782580L;

	public OWLEntityExistsException() {
	}

	public OWLEntityExistsException(String message) {
		super(message);
	}

	public OWLEntityExistsException(Throwable cause) {
		super(cause);
	}

	public OWLEntityExistsException(String message, Throwable cause) {
		super(message, cause);
	}

	public OWLEntityExistsException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
