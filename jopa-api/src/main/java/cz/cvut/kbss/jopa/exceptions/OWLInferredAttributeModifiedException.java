package cz.cvut.kbss.jopa.exceptions;

/**
 * This exception is raised when the user modifies an inferred attribute, which
 * is forbidden.
 * 
 * @author kidney
 * 
 */
public class OWLInferredAttributeModifiedException extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public OWLInferredAttributeModifiedException() {
	}

	public OWLInferredAttributeModifiedException(String message, Throwable cause) {
		super(message, cause);
	}

	public OWLInferredAttributeModifiedException(String message) {
		super(message);
	}

	public OWLInferredAttributeModifiedException(Throwable cause) {
		super(cause);
	}

}
