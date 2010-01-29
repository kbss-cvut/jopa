package cz.cvut.kbss.owlpersistence.model;

@SuppressWarnings("serial")
public class OWLPersistenceException extends RuntimeException {

	public OWLPersistenceException() {
	}

	public OWLPersistenceException(String message, Throwable cause) {
		super(message, cause);
	}

	public OWLPersistenceException(String message) {
		super(message);
	}

	public OWLPersistenceException(Throwable cause) {
		super(cause);
	}
}
