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

	@Override
	public int hashCode() {
		int prime = 31;
		return prime * super.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		return true;
	}
}
