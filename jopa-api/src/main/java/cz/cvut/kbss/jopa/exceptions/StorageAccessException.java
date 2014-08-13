package cz.cvut.kbss.jopa.exceptions;

/**
 * This exception is thrown when an exception occurs while accessing OntoDriver
 * from JOPA.
 * 
 * @author ledvima1
 * 
 */
public class StorageAccessException extends RuntimeException {

	private static final long serialVersionUID = 4661531292404254252L;

	public StorageAccessException() {
	}

	public StorageAccessException(String message) {
		super(message);
	}

	public StorageAccessException(Throwable cause) {
		super(cause);
	}

	public StorageAccessException(String message, Throwable cause) {
		super(message, cause);
	}
}
