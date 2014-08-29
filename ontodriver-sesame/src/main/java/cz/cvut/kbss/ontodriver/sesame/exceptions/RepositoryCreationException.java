package cz.cvut.kbss.ontodriver.sesame.exceptions;

public class RepositoryCreationException extends RuntimeException {

	private static final long serialVersionUID = -7270311225695090499L;

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
