package cz.cvut.kbss.ontodriver_new.model;

import cz.cvut.kbss.jopa.model.IRI;

/**
 * Represents named resources, i. e. resources identified by an IRI.
 * 
 * @author ledvima1
 * 
 */
public interface NamedResource {

	/**
	 * Gets identifier of this resource. </p>
	 * 
	 * @return IRI
	 */
	public IRI getIdentifier();
}
