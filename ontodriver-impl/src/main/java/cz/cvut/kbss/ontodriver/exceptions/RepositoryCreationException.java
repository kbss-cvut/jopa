package cz.cvut.kbss.ontodriver.exceptions;

/**
 * This exception is thrown when a local Sesame repository cannot be created for
 * some reason.
 * 
 * @author ledvima1
 * 
 */
public class RepositoryCreationException extends RuntimeException {

	private static final long serialVersionUID = 5974386203929950178L;

	public RepositoryCreationException() {
	}

	public RepositoryCreationException(String message) {
		super(message);
	}

	public RepositoryCreationException(Throwable cause) {
		super(cause);
	}

	public RepositoryCreationException(String message, Throwable cause) {
		super(message, cause);
	}
}
