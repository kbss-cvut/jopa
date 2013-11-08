package cz.cvut.kbss.jopa.exceptions;

public class OWLPersistentObjectException extends OWLPersistenceException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public OWLPersistentObjectException() {
		super();
	}

	public OWLPersistentObjectException(String message) {
		super(message);
	}

	public OWLPersistentObjectException(Throwable cause) {
		super(cause);
	}

	public OWLPersistentObjectException(String message, Throwable cause) {
		super(message, cause);
	}
}
