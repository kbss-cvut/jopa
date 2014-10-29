package cz.cvut.kbss.ontodriver_new.descriptors;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

/**
 * Represents values of a simple sequence.
 * 
 * @see SimpleListDescriptorImpl
 * @author kidney
 * 
 */
public class SimpleListValueDescriptor extends SimpleListDescriptorImpl implements
		ListValueDescriptor {

	private final List<NamedResource> values;

	public SimpleListValueDescriptor(NamedResource listOwner, Assertion listProperty,
			Assertion nextNodeProperty) {
		super(listOwner, listProperty, nextNodeProperty);
		this.values = new ArrayList<>();
	}

	@Override
	public List<NamedResource> getValues() {
		return Collections.unmodifiableList(values);
	}

	public void addValue(NamedResource value) {
		Objects.requireNonNull(value, ErrorUtils.constructNPXMessage("value"));
		values.add(value);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + values.hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (getClass() != obj.getClass())
			return false;
		SimpleListValueDescriptor other = (SimpleListValueDescriptor) obj;
		if (!descriptor.equals(other.descriptor)) {
			return false;
		}
		if (!values.equals(other.values))
			return false;
		return true;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("[SimpleListValueDescriptor: owner = ");
		sb.append(descriptor.getListOwner());
		sb.append(", values = ").append(values);
		sb.append("]");
		return sb.toString();
	}
}
