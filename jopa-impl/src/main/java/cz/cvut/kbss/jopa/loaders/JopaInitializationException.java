package cz.cvut.kbss.jopa.loaders;

/**
 * This exception is thrown when an error occurs during the initialization of
 * JOPA.
 * 
 * @author ledvima1
 * 
 */
public class JopaInitializationException extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public JopaInitializationException() {
	}

	public JopaInitializationException(String message) {
		super(message);
	}

	public JopaInitializationException(Throwable cause) {
		super(cause);
	}

	public JopaInitializationException(String message, Throwable cause) {
		super(message, cause);
	}
}
