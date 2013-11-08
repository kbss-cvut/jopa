package cz.cvut.kbss.jopa.exceptions;

public class NoUniqueResultException extends OWLPersistenceException {

	private static final long serialVersionUID = -6340735954399074847L;

	public NoUniqueResultException() {
	}

	public NoUniqueResultException(String message, Throwable cause) {
		super(message, cause);
	}

	public NoUniqueResultException(String message) {
		super(message);
	}

	public NoUniqueResultException(Throwable cause) {
		super(cause);
	}
}
