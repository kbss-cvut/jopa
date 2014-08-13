package cz.cvut.kbss.ontodriver_new.model;

import java.net.URI;

class ClassAssertion extends Assertion {

	private static final long serialVersionUID = 7238514047567604200L;

	ClassAssertion(URI assertionIdentifier, boolean isInferred) {
		super(assertionIdentifier, isInferred);
	}

	@Override
	public AssertionType getType() {
		return AssertionType.CLASS;
	}
}
