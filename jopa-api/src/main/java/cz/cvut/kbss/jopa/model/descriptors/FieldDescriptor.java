package cz.cvut.kbss.jopa.model.descriptors;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Objects;
import java.util.Set;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

/**
 * Describes a singular data property or a plural data, object or annotation
 * property field.
 * 
 * @author ledvima1
 * 
 */
public class FieldDescriptor extends Descriptor {

	private static final long serialVersionUID = 9210702436096009355L;

	private final Field field;

	public FieldDescriptor(Field attribute) {
		super();
		Objects.requireNonNull(attribute, ErrorUtils.constructNPXMessage("attribute"));
		this.field = attribute;
	}

	public FieldDescriptor(URI context, Field attribute) {
		super(context);
		Objects.requireNonNull(attribute, ErrorUtils.constructNPXMessage("attribute"));
		this.field = attribute;
	}

	@Override
	public Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute) {
		return getFieldDescriptor(attribute.getJavaField());
	}

	@Override
	public void addAttributeDescriptor(Field attribute, Descriptor descriptor) {
		// Do nothing
	}

	@Override
	public void addAttributeContext(Field attribute, URI context) {
		// Do nothing
	}

	private Descriptor getFieldDescriptor(Field field) {
		if (this.field.equals(field)) {
			return this;
		}
		throw new IllegalArgumentException("This field descriptor does not describe field " + field);
	}

	@Override
	protected Set<URI> getContextsInternal(Set<URI> contexts, Set<Descriptor> visited) {
		if (context == null) {
			return null;
		}
		contexts.add(context);
		visited.add(this);
		return contexts;
	}

	Field getField() {
		return field;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + field.hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		FieldDescriptor other = (FieldDescriptor) obj;
		if (!field.equals(other.field))
			return false;
		return true;
	}
}
