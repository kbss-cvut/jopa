package cz.cvut.kbss.ontodriver.exception;

/**
 * Thrown when null primary key is encountered during persist and the id field
 * is not auto generated.
 * 
 * @author kidney
 * 
 */
public class PrimaryKeyNotSetException extends RuntimeException {

	private static final long serialVersionUID = -6002772344822347746L;

	public PrimaryKeyNotSetException() {
		super();
	}

	public PrimaryKeyNotSetException(String message) {
		super(message);
	}
}
