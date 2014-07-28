package cz.cvut.kbss.jopa.model.descriptors;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Objects;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
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

	public FieldDescriptor(URI context, TypesSpecification<?, ?> types) {
		super(context);
		Objects.requireNonNull(types, ErrorUtils.constructNPXMessage("types"));
		this.field = types.getJavaField();
	}

	public FieldDescriptor(URI context, PropertiesSpecification<?, ?> properties) {
		super(context);
		Objects.requireNonNull(properties, ErrorUtils.constructNPXMessage("properties"));
		this.field = properties.getJavaField();
	}

	@Override
	public Descriptor getAttributeDescriptor(Attribute<?, ?> attribute) {
		return getFieldDescriptor(attribute.getJavaField());
	}

	@Override
	public Descriptor getTypesDescriptor(TypesSpecification<?, ?> types) {
		return getFieldDescriptor(types.getJavaField());
	}

	@Override
	public Descriptor getPropertiesDescriptor(PropertiesSpecification<?, ?> properties) {
		return getFieldDescriptor(properties.getJavaField());
	}

	private Descriptor getFieldDescriptor(Field field) {
		if (!this.field.equals(field)) {
			throw new IllegalArgumentException("This field descriptor does not describe field "
					+ field);
		}
		return this;
	}
}
