package cz.cvut.kbss.ontodriver_new;

import java.net.URI;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

/**
 * Describes a simple sequence. </p>
 * 
 * Simple lists are classic Lips-style lists (singly-linked lists), where each
 * node is a subject for axiom referencing the next node.
 * 
 * @author ledvima1
 * 
 */
public class SimpleListDescriptor {

	private final NamedResource listOwner;
	private final Assertion listProperty;
	private final Assertion nextNode;

	private URI context;

	public SimpleListDescriptor(NamedResource listOwner, Assertion listProperty,
			Assertion nextNodeProperty) {
		this.listOwner = Objects.requireNonNull(listOwner,
				ErrorUtils.constructNPXMessage("listOwner"));
		this.listProperty = Objects.requireNonNull(listProperty,
				ErrorUtils.constructNPXMessage("listProperty"));
		this.nextNode = Objects.requireNonNull(nextNodeProperty,
				ErrorUtils.constructNPXMessage("nextNode"));
	}

	/**
	 * Sets context in which the list is stored.
	 * 
	 * @param context
	 *            Context URI, may be {@code null}, representing the default
	 *            context
	 */
	public void setContext(URI context) {
		this.context = context;
	}

	/**
	 * Gets context in which the list is stored.
	 * 
	 * @return Context URI
	 */
	public URI getContext() {
		return context;
	}

	/**
	 * Gets owner of the list. </p>
	 * 
	 * That is, the named resource which is at the head of the list. In object
	 * model, it is the owning entity.
	 * 
	 * @return List owner
	 */
	public NamedResource getListOwner() {
		return listOwner;
	}

	/**
	 * Gets the property assertion which connects the list to its owner.
	 * 
	 * @return Property assertion
	 * @see #getListOwner()
	 */
	public Assertion getListProperty() {
		return listProperty;
	}

	/**
	 * Gets the property assertion which connects the list nodes to each other.
	 * 
	 * @return Property assertion
	 */
	public Assertion getNextNode() {
		return nextNode;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + listOwner.hashCode();
		result = prime * result + listProperty.hashCode();
		result = prime * result + nextNode.hashCode();
		result = prime * result + ((context == null) ? 0 : context.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof SimpleListDescriptor))
			return false;
		SimpleListDescriptor other = (SimpleListDescriptor) obj;
		if (!other.canEqual(this)) {
			return false;
		}
		if (!listOwner.equals(other.listOwner))
			return false;
		if (!listProperty.equals(other.listProperty))
			return false;
		if (!nextNode.equals(other.nextNode))
			return false;
		if (context == null) {
			if (other.context != null)
				return false;
		} else if (!context.equals(other.context))
			return false;
		return true;
	}

	/**
	 * Used to verify that the two instances can be compared using equals. </p>
	 * 
	 * See {@link http://www.artima.com/lejava/articles/equality.html} for more
	 * information.
	 * 
	 * @param other
	 *            The other instance
	 * @return whether the instances can be compared
	 */
	protected boolean canEqual(Object other) {
		return (other instanceof SimpleListDescriptor);
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("[SimpleList: owner = ");
		sb.append(listOwner).append(", list = ");
		sb.append(listProperty).append(", next node = ");
		sb.append(nextNode).append(", context = ");
		sb.append(context).append("]");
		return sb.toString();
	}
}
