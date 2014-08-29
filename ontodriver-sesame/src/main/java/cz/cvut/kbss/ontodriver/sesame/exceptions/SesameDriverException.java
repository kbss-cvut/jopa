package cz.cvut.kbss.ontodriver.sesame.exceptions;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * This is a generic checked exception thrown by the Sesame driver. </p>
 * 
 * It extends the {@link OntoDriverException}, so we can throw it instead of
 * that one.
 * 
 * @author ledvima1
 * 
 */
public class SesameDriverException extends OntoDriverException {

	private static final long serialVersionUID = 4771575441559318165L;

	public SesameDriverException() {
	}

	public SesameDriverException(String message) {
		super(message);
	}

	public SesameDriverException(Throwable cause) {
		super(cause);
	}

	public SesameDriverException(String message, Throwable cause) {
		super(message, cause);
	}
}
