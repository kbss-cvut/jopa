package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.model.Assertion;

abstract class DataPropertyFieldStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

    DataPropertyFieldStrategy(EntityType<X> et, Attribute<? super X, ?> att, Descriptor attributeDescriptor,
                              EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
    }

    boolean isValidRange(Object value) {
        return attribute.getJavaType().isAssignableFrom(value.getClass()) || isFieldEnum();
    }

    boolean isFieldEnum() {
        final Class<?> cls = attribute.getJavaField().getType();
        return cls.isEnum();
    }

    Object resolveEnumValue(Object value) {
        final Class cls = attribute.getJavaField().getType();
        return Enum.valueOf(cls, value.toString());
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
    }
}
