package cz.cvut.kbss.ontodriver.exception;

/**
 * Represents exception raised during the driver initialization.
 * 
 * @author kidney
 * 
 */
public class OntoDriverInitializationException extends RuntimeException {

	private static final long serialVersionUID = 9036747807320681068L;

	public OntoDriverInitializationException() {
		super();
	}

	public OntoDriverInitializationException(String message) {
		super(message);
	}

	public OntoDriverInitializationException(Throwable cause) {
		super(cause);
	}

	public OntoDriverInitializationException(String message, Throwable cause) {
		super(message, cause);
	}
}
