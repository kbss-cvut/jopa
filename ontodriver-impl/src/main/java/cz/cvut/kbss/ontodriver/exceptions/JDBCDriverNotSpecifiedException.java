package cz.cvut.kbss.ontodriver.exceptions;

public class JDBCDriverNotSpecifiedException extends RuntimeException {

	private static final long serialVersionUID = 5161054466719487109L;

	public JDBCDriverNotSpecifiedException() {
	}

	public JDBCDriverNotSpecifiedException(String message) {
		super(message);
	}

	public JDBCDriverNotSpecifiedException(Throwable cause) {
		super(cause);
	}

	public JDBCDriverNotSpecifiedException(String message, Throwable cause) {
		super(message, cause);
	}
}
