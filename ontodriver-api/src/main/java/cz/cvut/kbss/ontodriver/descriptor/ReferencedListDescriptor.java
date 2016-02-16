package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;

public interface ReferencedListDescriptor extends ListDescriptor {

	/**
	 * Gets the property assertion which represents each node's content.
	 * 
	 * @return Property assertion
	 */
	public abstract Assertion getNodeContent();
}