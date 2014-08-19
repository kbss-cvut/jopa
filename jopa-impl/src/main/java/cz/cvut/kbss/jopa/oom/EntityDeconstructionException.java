package cz.cvut.kbss.jopa.oom;

public class EntityDeconstructionException extends RuntimeException {

	private static final long serialVersionUID = -2448114736527027474L;

	public EntityDeconstructionException() {
	}

	public EntityDeconstructionException(String message) {
		super(message);
	}

	public EntityDeconstructionException(Throwable cause) {
		super(cause);
	}

	public EntityDeconstructionException(String message, Throwable cause) {
		super(message, cause);
	}
}
