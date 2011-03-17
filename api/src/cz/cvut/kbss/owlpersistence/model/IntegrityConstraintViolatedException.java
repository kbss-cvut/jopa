package cz.cvut.kbss.owlpersistence.model;

@SuppressWarnings("serial")
public class IntegrityConstraintViolatedException extends OWLPersistenceException {

	public IntegrityConstraintViolatedException() {
	}

	public IntegrityConstraintViolatedException(String message, Throwable cause) {
		super(message, cause);
	}

	public IntegrityConstraintViolatedException(String message) {
		super(message);
	}

	public IntegrityConstraintViolatedException(Throwable cause) {
		super(cause);
	}
}
