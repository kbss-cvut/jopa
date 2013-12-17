package cz.cvut.kbss.ontodriver.exceptions;

/**
 * Thrown if an error occurs when working with simple list.
 * 
 * @author ledvima1
 * 
 */
public class OWLSimpleListException extends RuntimeException {

	private static final long serialVersionUID = -1792958152792407568L;

	public OWLSimpleListException() {
	}

	public OWLSimpleListException(String message) {
		super(message);
	}

	public OWLSimpleListException(Throwable cause) {
		super(cause);
	}

	public OWLSimpleListException(String message, Throwable cause) {
		super(message, cause);
	}
}
