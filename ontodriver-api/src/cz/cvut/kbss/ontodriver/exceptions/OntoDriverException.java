package cz.cvut.kbss.ontodriver.exceptions;

import java.io.Serializable;

/**
 * Describes a general purpose exception that can be thrown by the OntoDriver.
 * </p>
 * 
 * This exception is likely to wrap another more specific exception (the cause).
 * 
 * @author kidney
 * 
 */
public class OntoDriverException extends Exception implements Serializable {

	private static final long serialVersionUID = 5057709405049286475L;

	public OntoDriverException() {
	}

	public OntoDriverException(String message) {
		super(message);
	}

	public OntoDriverException(Throwable cause) {
		super(cause);
	}

	public OntoDriverException(String message, Throwable cause) {
		super(message, cause);
	}

	public OntoDriverException(String message, Throwable cause,
			boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
