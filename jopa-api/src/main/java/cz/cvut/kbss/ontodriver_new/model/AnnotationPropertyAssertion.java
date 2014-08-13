package cz.cvut.kbss.ontodriver_new.model;

import java.net.URI;

final class AnnotationPropertyAssertion extends Assertion {

	private static final long serialVersionUID = 6708237135179035172L;

	AnnotationPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
		super(assertionIdentifier, isInferred);
	}

	@Override
	public AssertionType getType() {
		return AssertionType.ANNOTATION_PROPERTY;
	}
}
