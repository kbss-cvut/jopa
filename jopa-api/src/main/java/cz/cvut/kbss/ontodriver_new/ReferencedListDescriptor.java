package cz.cvut.kbss.ontodriver_new;

import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

/**
 * Represents singly-linked referenced list. </p>
 * 
 * In referenced list each node has content linked to it by a separate property.
 * In addition, each node points to its successor in the sequence (with the
 * exception of the last node, which has no successor).
 * 
 * @author ledvima1
 * 
 */
public class ReferencedListDescriptor extends SimpleListDescriptor {

	private final Assertion nodeContent;

	public ReferencedListDescriptor(NamedResource listOwner, Assertion listProperty,
			Assertion nextNode, Assertion nodeContent) {
		super(listOwner, listProperty, nextNode);
		this.nodeContent = Objects.requireNonNull(nodeContent,
				ErrorUtils.constructNPXMessage("nodeContent"));
	}

	/**
	 * Gets the property assertion which represents each node's content.
	 * 
	 * @return Property assertion
	 */
	public Assertion getNodeContent() {
		return nodeContent;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + nodeContent.hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof ReferencedListDescriptor))
			return false;
		ReferencedListDescriptor other = (ReferencedListDescriptor) obj;
		if (!other.canEqual(this)) {
			return false;
		}
		if (!nodeContent.equals(other.nodeContent))
			return false;
		if (!super.equals(other)) {
			return false;
		}
		return true;
	}

	protected boolean canEqual(Object other) {
		return (other instanceof ReferencedListDescriptor);
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("[ReferencedList: owner = ");
		sb.append(getListOwner()).append(", list = ");
		sb.append(getListProperty()).append(", next node = ");
		sb.append(getNextNode()).append(", node content = ");
		sb.append(getNodeContent()).append(", context = ");
		sb.append(getContext()).append("]");
		return sb.toString();
	}
}
