package cz.cvut.kbss.ontodriver_new.descriptors;

import java.net.URI;

import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

/**
 * This interface declares the basic methods for working with sequences in JOPA.
 * @author kidney
 *
 */
public interface ListDescriptor {

	/**
	 * Gets context in which the list is stored.
	 * 
	 * @return Context URI
	 */
	public abstract URI getContext();

	/**
	 * Sets context of the list.
	 * 
	 * @param context
	 *            Context URI, can be {@code null}
	 */
	public abstract void setContext(URI context);

	/**
	 * Gets owner of the list. </p>
	 * 
	 * That is, the named resource which is at the head of the list. In object
	 * model, it is the owning entity.
	 * 
	 * @return List owner
	 */
	public abstract NamedResource getListOwner();

	/**
	 * Gets the property assertion which connects the list to its owner.
	 * 
	 * @return Property assertion
	 * @see #getListOwner()
	 */
	public abstract Assertion getListProperty();

	/**
	 * Gets the property assertion which connects the list nodes to each other.
	 * 
	 * @return Property assertion
	 */
	public abstract Assertion getNextNode();

}