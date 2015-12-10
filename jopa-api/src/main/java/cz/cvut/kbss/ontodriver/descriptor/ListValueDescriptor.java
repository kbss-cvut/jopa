package cz.cvut.kbss.ontodriver.descriptor;

import java.util.List;

import cz.cvut.kbss.ontodriver.model.NamedResource;

/**
 * Descriptor for list values.
 * 
 * @author ledvima1
 * 
 */
public interface ListValueDescriptor extends ListDescriptor {

	/**
	 * Gets values from the list described by this descriptor.
	 * 
	 * @return List of value identifiers
	 */
	public List<NamedResource> getValues();

	/**
	 * Adds value to this list descriptor.
	 * 
	 * @param elem
	 *            The value to add, i. e. identifier of the list element
	 */
	public void addValue(NamedResource elem);
}
