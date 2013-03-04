package cz.cvut.kbss.ontodriver.exceptions;

public class IntegrityConstraintViolatedException extends RuntimeException {

	private static final long serialVersionUID = 8125380134315804076L;

	public IntegrityConstraintViolatedException() {
		super();
	}

	public IntegrityConstraintViolatedException(String message) {
		super(message);
	}

	public IntegrityConstraintViolatedException(Throwable cause) {
		super(cause);
	}

	public IntegrityConstraintViolatedException(String message, Throwable cause) {
		super(message, cause);
	}

	public IntegrityConstraintViolatedException(String message, Throwable cause,
			boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
