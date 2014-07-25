package cz.cvut.kbss.jopa.model.descriptors;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
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
	public EntityDescriptor addAttributeDescriptor(Attribute<?, ?> attribute, Descriptor descriptor) {
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
	public EntityDescriptor addAttributeDescriptor(Attribute<?, ?> attribute, URI context) {
		Objects.requireNonNull(attribute, ErrorUtils.constructNPXMessage("attribute"));

		fieldDescriptors.put(attribute.getJavaField(), createDescriptor(attribute, context));
		return this;
	}

	@Override
	public Descriptor getAttributeDescriptor(Attribute<?, ?> attribute) {
		for (Entry<Field, Descriptor> e : fieldDescriptors.entrySet()) {
			if (e.getKey().equals(attribute)) {
				return e.getValue();
			}
		}
		// Use our context
		return createDescriptor(attribute, context);
	}

	private static Descriptor createDescriptor(Attribute<?, ?> att, URI context) {
		if (att.getPersistentAttributeType() == Attribute.PersistentAttributeType.OBJECT
				&& !att.isCollection()) {
			return new EntityDescriptor(context);
		} else {
			return new FieldDescriptor(context, att);
		}
	}
}
