package cz.cvut.kbss.jopa.model.descriptors;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

/**
 * Describes an entity. </p>
 * 
 * Each attribute has a descriptor associated with it.
 * 
 * @author ledvima1
 * 
 */
public class EntityDescriptor extends Descriptor {

	private static final long serialVersionUID = -7335572526695707060L;

	private final Map<Field, Descriptor> fieldDescriptors;

	public EntityDescriptor() {
		this.fieldDescriptors = new HashMap<>();
	}

	public EntityDescriptor(URI context) {
		super(context);
		this.fieldDescriptors = new HashMap<>();
	}

	/**
	 * Adds the specified descriptor to this EntityDescriptor. </p>
	 * 
	 * @param attribute
	 *            Attribute which the specified descriptor describes
	 * @param descriptor
	 *            The descriptor to add
	 * @return This instance
	 */
	public EntityDescriptor addAttributeDescriptor(FieldSpecification<?, ?> attribute,
			Descriptor descriptor) {
		Objects.requireNonNull(attribute, ErrorUtils.constructNPXMessage("attribute"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		fieldDescriptors.put(attribute.getJavaField(), descriptor);
		return this;
	}

	/**
	 * Creates and adds a descriptor to this EntityDescriptor. </p>
	 * 
	 * @param attribute
	 *            Attribute which the specified descriptor describes
	 * @param context
	 *            Context information for the new descriptor. This can be
	 *            {@code null}, meaning that the default context is referenced
	 * @return This instance
	 */
	public EntityDescriptor addAttributeDescriptor(FieldSpecification<?, ?> attribute, URI context) {
		Objects.requireNonNull(attribute, ErrorUtils.constructNPXMessage("attribute"));

		fieldDescriptors.put(attribute.getJavaField(), createDescriptor(attribute, context));
		return this;
	}

	@Override
	public Descriptor getAttributeDescriptor(FieldSpecification<?, ?> attribute) {
		Descriptor d = getFieldDescriptor(attribute.getJavaField());
		if (d == null) {
			d = createDescriptor(attribute, context);
		}
		return d;
	}

	private Descriptor getFieldDescriptor(Field field) {
		for (Entry<Field, Descriptor> e : fieldDescriptors.entrySet()) {
			if (e.getKey().equals(field)) {
				return e.getValue();
			}
		}
		return null;
	}

	private static Descriptor createDescriptor(FieldSpecification<?, ?> att, URI context) {
		if ((att instanceof TypesSpecification<?, ?>)
				|| (att instanceof PropertiesSpecification<?, ?>)) {
			return new FieldDescriptor(context, att);
		}
		final Attribute<?, ?> attSpec = (Attribute<?, ?>) att;
		if (attSpec.getPersistentAttributeType() == PersistentAttributeType.OBJECT
				&& !attSpec.isCollection()) {
			return new EntityDescriptor(context);
		}
		return new FieldDescriptor(context, att);
	}

	@Override
	protected Set<URI> getContextsInternal(Set<URI> contexts, Set<Descriptor> visited) {
		if (visited.contains(this)) {
			return contexts;
		}
		if (context == null) {
			return null;
		}
		contexts.add(context);
		visited.add(this);
		for (Descriptor fd : fieldDescriptors.values()) {
			contexts = fd.getContextsInternal(contexts, visited);
			if (contexts == null) {
				return contexts;
			}
		}
		return contexts;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((fieldDescriptors == null) ? 0 : fieldDescriptors.hashCode());
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
		EntityDescriptor other = (EntityDescriptor) obj;
		if (fieldDescriptors == null) {
			if (other.fieldDescriptors != null)
				return false;
		} else if (!fieldDescriptors.equals(other.fieldDescriptors))
			return false;
		return true;
	}

}
