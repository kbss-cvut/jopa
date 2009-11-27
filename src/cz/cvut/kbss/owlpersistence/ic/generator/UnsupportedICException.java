package cz.cvut.kbss.owlpersistence.ic.generator;

public class UnsupportedICException extends Exception {

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
