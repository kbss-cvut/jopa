package cz.cvut.kbss.ontodriver.sesame.exceptions;

public class RepositoryNotFoundException extends RuntimeException {

	private static final long serialVersionUID = 4908217935798431138L;

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
