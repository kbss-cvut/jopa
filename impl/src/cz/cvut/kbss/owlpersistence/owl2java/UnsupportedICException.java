package cz.cvut.kbss.owlpersistence.owl2java;

public class UnsupportedICException extends RuntimeException {

	public UnsupportedICException(final Throwable t) {
		super(t);
	}

	public UnsupportedICException(final String t) {
		super(t);
	}

	public UnsupportedICException(final String t, final Throwable tt) {
		super(t, tt);
	}
}
