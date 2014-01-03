package cz.cvut.kbss.ontodriver.exceptions;

/**
 * Thrown if an error occurs when working with a referenced list.
 * 
 * @author ledvima1
 * 
 */
public class OWLReferencedListException extends RuntimeException {

	private static final long serialVersionUID = -7296679317097061902L;

	public OWLReferencedListException() {
	}

	public OWLReferencedListException(String message) {
		super(message);
	}

	public OWLReferencedListException(Throwable cause) {
		super(cause);
	}

	public OWLReferencedListException(String message, Throwable cause) {
		super(message, cause);
	}
}
