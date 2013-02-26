package cz.cvut.kbss.ontodriver.impl.owlapi;

public class OwlModuleException extends RuntimeException {

	private static final long serialVersionUID = 4041599912127241250L;

	public OwlModuleException() {
		super();
	}

	public OwlModuleException(String message) {
		super(message);
	}

	public OwlModuleException(Throwable cause) {
		super(cause);
	}

	public OwlModuleException(String message, Throwable cause) {
		super(message, cause);
	}

	public OwlModuleException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
