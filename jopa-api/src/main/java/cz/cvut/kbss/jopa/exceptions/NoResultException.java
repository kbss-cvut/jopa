package cz.cvut.kbss.jopa.exceptions;

public class NoResultException extends OWLPersistenceException {

	private static final long serialVersionUID = -1891852675684320722L;

	public NoResultException() {
	}

	public NoResultException(String message, Throwable cause) {
		super(message, cause);
	}

	public NoResultException(String message) {
		super(message);
	}

	public NoResultException(Throwable cause) {
		super(cause);
	}
}
