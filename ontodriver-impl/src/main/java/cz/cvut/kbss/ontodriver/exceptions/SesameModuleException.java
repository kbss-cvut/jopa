package cz.cvut.kbss.ontodriver.exceptions;

/**
 * Generic exception thrown by the Sesame module implementation.
 * 
 * @author ledvima1
 * 
 */
public class SesameModuleException extends OntoDriverInternalException {

	private static final long serialVersionUID = 9086575899877058273L;

	public SesameModuleException() {
	}

	public SesameModuleException(String message) {
		super(message);
	}

	public SesameModuleException(Throwable cause) {
		super(cause);
	}

	public SesameModuleException(String message, Throwable cause) {
		super(message, cause);
	}
}
