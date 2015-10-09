package cz.cvut.kbss.ontodriver_new.exception;

public class IdentifierGenerationException extends RuntimeException {

	private static final long serialVersionUID = -8466375018813594628L;

	public IdentifierGenerationException() {
	}

	public IdentifierGenerationException(String message) {
		super(message);
	}

	public IdentifierGenerationException(Throwable cause) {
		super(cause);
	}

	public IdentifierGenerationException(String message, Throwable cause) {
		super(message, cause);
	}
}
