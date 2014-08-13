package cz.cvut.kbss.ontodriver_new.model;

import java.net.URI;

final class DataPropertyAssertion extends Assertion {

	private static final long serialVersionUID = -7925376004449347678L;

	DataPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
		super(assertionIdentifier, isInferred);
	}

	@Override
	public AssertionType getType() {
		return AssertionType.DATA_PROPERTY;
	}
}
