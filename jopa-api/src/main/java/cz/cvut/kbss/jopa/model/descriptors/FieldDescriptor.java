package cz.cvut.kbss.jopa.model.descriptors;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Objects;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
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

	public FieldDescriptor(Attribute<?, ?> attribute) {
		super();
		Objects.requireNonNull(attribute, ErrorUtils.constructNPXMessage("attribute"));
		this.field = attribute.getJavaField();
	}

	public FieldDescriptor(URI context, Attribute<?, ?> attribute) {
		super(context);
		Objects.requireNonNull(attribute, ErrorUtils.constructNPXMessage("attribute"));
		this.field = attribute.getJavaField();
	}

	@Override
	public Descriptor getAttributeDescriptor(Attribute<?, ?> attribute) {
		if (!this.field.equals(attribute.getJavaField())) {
			throw new IllegalArgumentException("This field descriptor does not describe field "
					+ attribute);
		}
		return this;
	}
}
