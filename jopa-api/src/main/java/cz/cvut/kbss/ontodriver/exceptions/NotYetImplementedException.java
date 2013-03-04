package cz.cvut.kbss.ontodriver.exceptions;

/**
 * Exception thrown for features that are not implemented yet.
 * 
 * @author kidney
 * 
 */
public class NotYetImplementedException extends RuntimeException {

	private static final long serialVersionUID = -8127692503618130945L;

	public NotYetImplementedException() {
		super("Not yet implemented.");
	}

	public NotYetImplementedException(String message) {
		super(message);
	}
}
