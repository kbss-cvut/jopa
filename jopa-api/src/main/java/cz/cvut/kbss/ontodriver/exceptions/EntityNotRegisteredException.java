package cz.cvut.kbss.ontodriver.exceptions;

/**
 * This exception is throws when an entity is not registered with a
 * {@code Connection}.
 * 
 * @author kidney
 * 
 */
public class EntityNotRegisteredException extends RuntimeException {

	private static final long serialVersionUID = -1219039801652839793L;

	public EntityNotRegisteredException() {
	}

	public EntityNotRegisteredException(String message) {
		super(message);
	}

	public EntityNotRegisteredException(Throwable cause) {
		super(cause);
	}

	public EntityNotRegisteredException(String message, Throwable cause) {
		super(message, cause);
	}

	public EntityNotRegisteredException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
