package cz.cvut.kbss.ontodriver_new.model;

import java.net.URI;

final class ObjectPropertyAssertion extends Assertion {

	private static final long serialVersionUID = 9210709887861831464L;

	ObjectPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
		super(assertionIdentifier, isInferred);

	}

	@Override
	public AssertionType getType() {
		return AssertionType.OBJECT_PROPERTY;
	}
}
