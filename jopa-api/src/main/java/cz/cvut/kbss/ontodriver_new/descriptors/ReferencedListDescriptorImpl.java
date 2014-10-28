package cz.cvut.kbss.ontodriver_new.descriptors;

import java.net.URI;
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
public class ReferencedListDescriptorImpl implements ReferencedListDescriptor {

	protected final ListDescriptor descriptor;
	private final Assertion nodeContent;

	public ReferencedListDescriptorImpl(NamedResource listOwner, Assertion listProperty,
			Assertion nextNode, Assertion nodeContent) {
		this.descriptor = new BaseListDescriptorImpl(listOwner, listProperty, nextNode);
		this.nodeContent = Objects.requireNonNull(nodeContent,
				ErrorUtils.constructNPXMessage("nodeContent"));
	}

	@Override
	public URI getContext() {
		return descriptor.getContext();
	}

	@Override
	public void setContext(URI context) {
		descriptor.setContext(context);
	}

	@Override
	public NamedResource getListOwner() {
		return descriptor.getListOwner();
	}

	@Override
	public Assertion getListProperty() {
		return descriptor.getListProperty();
	}

	@Override
	public Assertion getNextNode() {
		return descriptor.getNextNode();
	}

	@Override
	public Assertion getNodeContent() {
		return nodeContent;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + descriptor.hashCode();
		result = prime * result + nodeContent.hashCode();
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
		ReferencedListDescriptorImpl other = (ReferencedListDescriptorImpl) obj;
		if (!descriptor.equals(other.descriptor))
			return false;
		if (!nodeContent.equals(other.nodeContent))
			return false;
		return true;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("[ReferencedList: ");
		sb.append(descriptor).append(", nodeContent = ");
		sb.append(nodeContent).append("]");
		return sb.toString();
	}
}
