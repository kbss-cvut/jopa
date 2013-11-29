package cz.cvut.kbss.ontodriver.exceptions;

/**
 * Thrown when a remote ontology repository (storage) cannot be reached.
 * 
 * @author ledvima1
 * 
 */
public class RepositoryNotFoundException extends RuntimeException {

	private static final long serialVersionUID = -7793803677091750343L;

	public RepositoryNotFoundException() {
	}

	public RepositoryNotFoundException(String message) {
		super(message);
	}

	public RepositoryNotFoundException(Throwable cause) {
		super(cause);
	}

	public RepositoryNotFoundException(String message, Throwable cause) {
		super(message, cause);
	}
}
