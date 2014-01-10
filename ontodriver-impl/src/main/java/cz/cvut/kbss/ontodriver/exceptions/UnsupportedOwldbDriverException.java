package cz.cvut.kbss.ontodriver.exceptions;

public class UnsupportedOwldbDriverException extends RuntimeException {

	private static final long serialVersionUID = -4452819081441097459L;

	public UnsupportedOwldbDriverException() {
	}

	public UnsupportedOwldbDriverException(String message) {
		super(message);
	}

	public UnsupportedOwldbDriverException(Throwable cause) {
		super(cause);
	}

	public UnsupportedOwldbDriverException(String message, Throwable cause) {
		super(message, cause);
	}
}
