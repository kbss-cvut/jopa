package cz.cvut.kbss.ontodriver_new.descriptors;

import cz.cvut.kbss.ontodriver_new.model.Assertion;

public interface ReferencedListDescriptor extends ListDescriptor {

	/**
	 * Gets the property assertion which represents each node's content.
	 * 
	 * @return Property assertion
	 */
	public abstract Assertion getNodeContent();
}