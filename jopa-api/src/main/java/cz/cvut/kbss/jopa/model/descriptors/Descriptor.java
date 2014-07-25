package cz.cvut.kbss.jopa.model.descriptors;

import java.io.Serializable;
import java.net.URI;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

/**
 * Defines base descriptor, which is used to specify context information for
 * entities and their fields. </p>
 * 
 * The descriptor hierarchy is a classical <b>Composite</b> pattern.
 * 
 * @author ledvima1
 * 
 */
public abstract class Descriptor implements Serializable {

	private static final long serialVersionUID = 3916845634058702888L;

	protected final URI context;

	protected Descriptor() {
		this.context = null;
	}

	protected Descriptor(URI context) {
		this.context = context;
	}

	/**
	 * Gets context for this descriptor. </p>
	 * 
	 * Note that the context URI may be {@code null}, meaning that the default
	 * context is referenced
	 * 
	 * @return Context URI
	 */
	public URI getContext() {
		return context;
	}

	/**
	 * Gets descriptor for the specified attribute. </p>
	 * 
	 * @param attribute
	 *            Entity attribute, as specified by application metamodel
	 * @return Descriptor
	 * @throws IllegalArgumentException
	 *             If the descriptor is not available
	 */
	public abstract Descriptor getAttributeDescriptor(Attribute<?, ?> attribute);

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((context == null) ? 0 : context.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Descriptor other = (Descriptor) obj;
		if (context == null) {
			if (other.context != null)
				return false;
		} else if (!context.equals(other.context))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return context.toString();
	}
}
