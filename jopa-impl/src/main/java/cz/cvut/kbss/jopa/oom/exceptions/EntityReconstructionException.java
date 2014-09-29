package cz.cvut.kbss.jopa.oom.exceptions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

public class EntityReconstructionException extends OWLPersistenceException {

	private static final long serialVersionUID = 1L;

	public EntityReconstructionException() {
	}

	public EntityReconstructionException(String message, Throwable cause) {
		super(message, cause);
	}

	public EntityReconstructionException(String message) {
		super(message);
	}

	public EntityReconstructionException(Throwable cause) {
		super(cause);
	}
}
