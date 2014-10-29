package cz.cvut.kbss.ontodriver_new.descriptors;

import java.util.List;

import cz.cvut.kbss.ontodriver_new.model.NamedResource;

/**
 * Descriptor for list values.
 * 
 * @author ledvima1
 * 
 */
public interface ListValueDescriptor {

	/**
	 * Gets values from the list described by this descriptor.
	 * 
	 * @return List of value identifiers
	 */
	public List<NamedResource> getValues();
}
