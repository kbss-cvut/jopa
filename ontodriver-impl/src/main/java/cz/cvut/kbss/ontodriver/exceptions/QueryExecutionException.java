package cz.cvut.kbss.ontodriver.exceptions;

public class QueryExecutionException extends RuntimeException {

	private static final long serialVersionUID = -430790435619535226L;

	public QueryExecutionException() {
	}

	public QueryExecutionException(String message) {
		super(message);
	}

	public QueryExecutionException(Throwable cause) {
		super(cause);
	}

	public QueryExecutionException(String message, Throwable cause) {
		super(message, cause);
	}
}
