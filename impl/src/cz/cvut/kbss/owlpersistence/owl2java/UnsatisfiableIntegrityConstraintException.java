package cz.cvut.kbss.owlpersistence.owl2java;

public class UnsatisfiableIntegrityConstraintException extends UnsupportedICException {

	public UnsatisfiableIntegrityConstraintException(final Throwable t) {
		super(t);
	}

	public UnsatisfiableIntegrityConstraintException(final String t) {
		super(t);
	}

	public UnsatisfiableIntegrityConstraintException(final String t, final Throwable tt) {
		super(t, tt);
	}
}
